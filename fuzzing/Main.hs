{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.DeepSeq (($!!), NFData)
import qualified Control.Exception as X
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Class (get, spawn)
import Control.Monad.Par.IO (runParIO)
import Data.List (isPrefixOf, partition, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat, Endo(..))
import Data.Time
  (defaultTimeLocale, formatTime, getZonedTime, iso8601DateFormat)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Console.GetOpt
  (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import System.Directory
  (copyFile, createDirectoryIfMissing, findExecutable, getFileSize,
   getPermissions, setPermissions, setOwnerExecutable, makeAbsolute)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>), (<.>), dropExtension)
import System.IO.Temp (withTempDirectory)
import System.Process
  (callProcess, readProcess, readProcessWithExitCode,
   spawnProcess, waitForProcess)
import System.Random (randomIO)
import Text.Read (readMaybe)
import Text.XML.Light (Attr(..), Element, ppTopElement, unode, unqual)

-- Option Parsing --------------------------------------------------------------

-- | The @clang@ executable and flags for a particular test
-- configuration, e.g., @("clang-3.8", "-O -w -g")@
type Clang = (FilePath, [String], String)

data Options = Options {
    optNumTests :: Integer
    -- ^ Number of Tests
  , optSeeds :: Maybe [Word64]
    -- ^ Specific seeds to use in fuzzing; overrides 'optNumTests'
  , optSaveTests :: Maybe FilePath
    -- ^ Location to save failed tests
  , optClangs :: [FilePath]
    -- ^ Clangs to use with the fuzzer
  , optClangFlags :: [String]
    -- ^ Sets of argument flags to use with each clang configuration
  , optIncludeDirs :: [String]
    -- ^ List of include directories to pass to clang, since those
    -- interact badly with intermediate file names
  , optJUnitXml :: Maybe FilePath
    -- ^ Write JUnit test report
  , optCsmithPath :: Maybe FilePath
    -- ^ Path to Csmith include files
  , optCollapse :: Bool
    -- ^ Whether to collapse failures with the same error message
  , optReduceDisasm :: Bool
    -- ^ Whether to try reducing failures at the disasm stage
  , optRunAs :: Bool
    -- ^ Whether to run the assembly stage
  , optReduceAs :: Bool
    -- ^ Whether to try reducing failures at the assembly stage
  , optRunExec :: Bool
    -- ^ Whether to run re-execution stage
  , optReduceExec :: Bool
    -- ^ Whether to try reducing failures at the execution stage
  , optHelp :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optNumTests     = 100
  , optSeeds        = Nothing
  , optSaveTests    = Nothing
  , optClangs       = ["clang"]
  , optClangFlags   = ["-O -g -w"]
  , optIncludeDirs  = []
  , optJUnitXml     = Nothing
  , optCsmithPath   = Nothing
  , optCollapse     = False
  , optReduceDisasm = False
  , optRunAs        = False
  , optReduceAs     = False
  , optRunExec      = False
  , optReduceExec   = False
  , optHelp         = False
  }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "n" [] (ReqArg setNumTests "NUMBER")
    "number of tests to run; default is 100"
  , Option "s" ["seed"] (ReqArg addSeed "SEED")
    "specific Csmith seed to use; overrides -n"
  , Option "o" ["output"] (ReqArg setSaveTests "DIRECTORY")
    "directory to save failed tests"
  , Option "c" ["clang"] (ReqArg addClang "CLANG")
    "specify clang executables to use, e.g., `-c clang-3.8 -c clang-3.9'"
  , Option ""  ["clang-flags"] (ReqArg addClangFlags "ARGS") $
    "specify a set of flags to use with each clang " ++
    "e.g., `--clang-flags \"-O\" --clang-flags \"-O -g\"'"
  , Option "I" ["include-dir"] (ReqArg addIncludeDir "DIRECTORY")
    "add a directory to the include search path for clang"
  , Option ""  ["junit-xml"] (ReqArg setJUnitXml "FILEPATH")
    "output JUnit-style XML test report"
  , Option ""  ["csmith-path"] (ReqArg setCsmithPath "DIRECTORY")
    "path to Csmith include files; default is $CSMITH_PATH environment variable"
  , Option ""  ["collapse"] (NoArg setCollapse)
    "collapse failing test cases by error message and remove successes"
  , Option ""  ["reduce-disasm"] (NoArg setReduceDisasm) $
    "reduce test cases that fail disassembly with a best-guess Creduce " ++
    "(requires `--output`)"
  , Option ""  ["run-as"] (NoArg setRunAs) $
    "run the re-assembly phase"
  , Option ""  ["reduce-as"] (NoArg setReduceAs) $
    "reduce test cases that fail reassembly with a best-guess Creduce " ++
    "(requires `--output`)"
  , Option ""  ["run-exec"] (NoArg setRunExec) $
    "try to execute the re-assembled program"
  , Option ""  ["reduce-exec"] (NoArg setReduceExec) $
    "reduce test cases that fail on re-assembled program execution with " ++
    "a best-guess Creduce (requires `--output`)"
  , Option "h" ["help"] (NoArg setHelp)
    "display this message"
  ]

setNumTests :: String -> Endo Options
setNumTests str = Endo $ \opt ->
  case readMaybe str of
    Nothing -> error "expected integer number of tests"
    Just n -> opt { optNumTests = n }

addSeed :: String -> Endo Options
addSeed str = Endo $ \opt ->
  case readMaybe str of
    Nothing -> error "expected 64-bit integer seed"
    Just n ->
      case optSeeds opt of
        Nothing    -> opt { optSeeds = Just [n] }
        Just seeds -> opt { optSeeds = Just (n : seeds) }

setSaveTests :: String -> Endo Options
setSaveTests str = Endo (\opt -> opt { optSaveTests = Just str })

addClang :: String -> Endo Options
addClang str = Endo $ \opt ->
  if optClangs opt == optClangs defaultOptions
  then opt { optClangs = [str] }
  else opt { optClangs = str : optClangs opt }

addClangFlags :: String -> Endo Options
addClangFlags str = Endo $ \opt ->
  if optClangFlags opt == optClangFlags defaultOptions
  then opt { optClangFlags = [str] }
  else opt { optClangFlags = str : optClangFlags opt }

addIncludeDir :: String -> Endo Options
addIncludeDir str = Endo $ \opt ->
  opt { optIncludeDirs = str : optIncludeDirs opt }

setJUnitXml :: String -> Endo Options
setJUnitXml str = Endo (\opt -> opt { optJUnitXml = Just str })

setCsmithPath :: String -> Endo Options
setCsmithPath str = Endo (\opt -> opt { optCsmithPath = Just str })

setCollapse :: Endo Options
setCollapse = Endo (\opt -> opt { optCollapse = True })

setReduceDisasm :: Endo Options
setReduceDisasm = Endo (\opt -> opt { optReduceDisasm = True })

setRunAs :: Endo Options
setRunAs = Endo (\opt -> opt { optRunAs = True })

setReduceAs :: Endo Options
setReduceAs = Endo (\opt -> opt { optReduceAs = True })

setRunExec :: Endo Options
setRunExec = Endo (\opt -> opt { optRunExec = True })

setReduceExec :: Endo Options
setReduceExec = Endo (\opt -> opt { optReduceExec = True })

setHelp :: Endo Options
setHelp = Endo (\opt -> opt { optHelp = True })

getOptions :: IO Options
getOptions =
  do args <- getArgs
     case getOpt RequireOrder options args of

       (fs,[],[]) -> do let opts = appEndo (mconcat fs) defaultOptions

                        when (optHelp opts) $ do printUsage []
                                                 exitSuccess

                        return opts

       (_,_,errs) -> do printUsage errs
                        exitFailure

printUsage :: [String] -> IO ()
printUsage errs =
  do prog <- getProgName
     let banner = "Usage: " ++ prog ++ " [OPTIONS]"
     putStrLn (usageInfo (unlines (errs ++ [banner])) options)

main :: IO ()
main = withTempDirectory "." ".fuzz." $ \tmpDir -> do
  opts <- getOptions
  let includeDirs = optIncludeDirs opts
  when (optSaveTests opts == Nothing &&
        or [optReduceDisasm opts, optReduceAs opts, optReduceExec opts]) $
    printUsage [ "--reduce options require --output to be set" ]
  -- run the tests within each clang version in parallel. We could
  -- parallelize the runs across clang versions as well, but it's
  -- probably not worth the complexity at that level of granularity
  liftIO $ putStrLn $ "Temp directory: " ++ tmpDir
  resultMaps <-
    forM (optClangs opts) $ \clangExe ->
    forM (optClangFlags opts) $ \flags -> runParIO $ do
      let clang = (clangExe, includeDirs, flags)
      liftIO $ putStrLn $ "[" ++ clangExe ++ " " ++ flags ++ "]"
      forM_ includeDirs $ \dir ->
        liftIO $ putStrLn $ "Include directory: " ++ dir
      results' <-
        case optSeeds opts of
          Nothing ->
            forM [1..optNumTests opts] $ \_ ->
              spawn $ liftIO $ do
              seed <- randomIO
              runTest tmpDir clang seed opts
          Just seeds ->
            forM seeds $ \seed ->
              spawn $ liftIO $ do
              runTest tmpDir clang seed opts
      results <- mapM get results'
      return (Map.singleton clang results)
  let allResults' = Map.unions (concat resultMaps)
      allResults | optCollapse opts = collapseResults allResults'
                 | otherwise        = allResults'
  forM_ (Map.toList allResults) $ \((clangExe, _, flags), results) -> do
    let (_passes, fails) = partition isPass results
    when (not (null fails)) $ do
      putStrLn $ "[" ++ clangExe ++ " " ++ flags ++ "] " ++
        show (length fails) ++ " failing cases identified:"
      forM_ fails $ \case
        TestFail st s _ _ -> putStrLn ("[" ++ show st ++ "] " ++ show s)
        _ -> error "non-fail cases in fails"
  case optSaveTests opts of
    Nothing -> return ()
    Just root -> do
      createDirectoryIfMissing False root
      forM_ (Map.toList allResults) $ \((clangExe, _, flags), results) ->
        when (not (null (filter isFail results))) $ do
          let clangRoot = root </> (clangExe ++ "_" ++ toUnders " " flags)
          createDirectoryIfMissing False clangRoot
          forM_ results $ \result ->
            case result of
              TestPass _ -> return ()
              -- errors arise from bugs in clang, not our code
              TestClangError _ -> return ()
              -- if the initial Csmith program doesn't terminate, don't bother
              TestExecTimeout _ -> return ()
              TestFail st _ TestSrc{..} err -> do
                copyFile (tmpDir </> srcFile) (clangRoot </> srcFile)
                writeFile (clangRoot </> srcFile <.> show st <.> "err") err
                let bcFile = dropExtension srcFile <.> "bc"
                copyFile (tmpDir </> bcFile) (clangRoot </> bcFile)
                when (or [ st == DisasmStage && optReduceDisasm opts
                         , st == AsStage     && optReduceAs opts
                         , st == ExecStage   && optReduceExec opts ]) $
                  reduce result (clangExe, includeDirs, flags) opts clangRoot
  case optJUnitXml opts of
    Nothing -> return ()
    Just f -> do
      xml <- mkJUnitXml allResults
      writeFile f (ppTopElement xml)

reduce :: TestResult -> Clang -> Options -> FilePath -> IO ()
reduce (TestFail st _ TestSrc{..} err) (clangExe, includeDirs, flags) opts clangRoot = do
  csmithPath <- getCsmithPath opts
  -- copy a file for the reduction in place
  let baseName   = dropExtension srcFile
      srcReduced = clangRoot </> baseName ++ "-reduced.c"
      scriptFile = clangRoot </> baseName ++ "-reduce.sh"
      llvmVersionFlags =
        case stripPrefix "clang-" clangExe of
          Nothing -> []
          Just ver -> [ "--llvm-version=" ++ ver ]
      includeOpts = concatMap (\dir -> ["-I", dir]) includeDirs
  copyFile (clangRoot </> srcFile) srcReduced
  absClangRoot <- makeAbsolute clangRoot
  let grepPat DisasmStage =
        -- look for the first line of the error starting with a
        -- tab; this should be the top of the llvm-disasm stack
        -- trace
        case dropWhile (not . isPrefixOf "\t") (lines err) of
          -- if we don't have a stack trace, we probably shouldn't
          -- waste our time
          [] -> Nothing
          -- drop the tab for the pattern
          first:_ -> Just (tail first)
      grepPat AsStage =
        -- check the first line of the clang error for "error:"
        case (lines err) of
          [] -> Nothing
          first:_ ->
            case dropWhile (/= "error:") (words first) of
              [] -> Nothing
              msg -> Just (unwords msg)
      grepPat ExecStage = error "no grep pattern for exec reduction"
      bcFile = baseName <.> "bc"
      llFile = baseName <.> "ll"
      scriptHeader = [
          "#!/usr/bin/env bash"
        , "# Consider this script a best guess template for reducing failing"
        , "# test cases. Not every bug will manifest in the same way and give"
        , "# the same error message, so modify the grep condition appropriately"
        , "# if the shrink is unsatisfactory."
        , ""
        , "set -e"
        , ""
        ]
      buildBc = unwords $ [
          clangExe, "-I", csmithPath, flags, "-c"
        , "-emit-llvm", baseName ++ "-reduced.c", "-o", bcFile
        ] ++ includeOpts
      buildLl = unwords $
          [ "llvm-disasm" ] ++
          llvmVersionFlags ++
          [ bcFile, ">", llFile ]
      copyBc = unwords [
          "cp", bcFile, absClangRoot </> bcFile
        ]
      script DisasmStage = unlines $ scriptHeader ++ [
          buildBc
        , copyBc
        , unwords $
            [ "llvm-disasm" ] ++
            llvmVersionFlags ++
            [ bcFile, "2>&1 |" , "grep", show (fromMaybe "" (grepPat st)) ]
        ]
      script AsStage = unlines $ scriptHeader ++ [
          buildBc
        , copyBc
        , buildLl
        , unwords [ clangExe, "-I", csmithPath, flags, "-c"
                  , llFile, "-o", baseName <.> "o", "2>&1 |"
                  , "fgrep ", show (fromMaybe "" (grepPat st))
                  ]
        ]
      script ExecStage = unlines $ scriptHeader ++ [
          buildBc
        , copyBc
        , buildLl
        , unwords [ clangExe, "-I", csmithPath, flags
                  , bcFile, "-o", "golden"
                  ]
        , unwords [ clangExe, "-I", csmithPath, flags
                  , llFile, "-o", "ours"
                  ]
        , "GOLDEN=$(timeout 10 ./golden)"
        , "[ \"$GOLDEN\" != \"$(./ours)\" ]"
        ]
  when (grepPat st /= Nothing) $ do
    -- write out the shell script to drive Creduce and run it
    writeFile scriptFile (script st)
    p <- getPermissions scriptFile
    setPermissions scriptFile (setOwnerExecutable True p)
    h <- spawnProcess "creduce" [ scriptFile, srcReduced ]
    void $ waitForProcess h

reduce _ _ _ _ = error "can't reduce non-failing test"

collapseResults :: Map Clang [TestResult] -> Map Clang [TestResult]
collapseResults = Map.map (collapse Map.empty)
  where
    collapse seen [] = Map.elems seen
    collapse seen (r:results) =
      case r of
        TestPass _ -> collapse seen results
        -- errors arise from bugs in clang, not our code
        TestClangError _ -> collapse seen results
        TestExecTimeout _ -> collapse seen results
        TestFail st _ _ err ->
          collapse (Map.insertWith f (st, err) r seen) results
    -- choose the smallest source file
    f r1@(TestFail _ _ src1 _) r2@(TestFail _ _ src2 _) =
      case compare (srcSize src1) (srcSize src2) of
        LT -> r1
        EQ -> r1 -- arbitrarily
        GT -> r2
    f _ _ = error "only test failures should go in this map"

type Seed = Word64

data TestResult
  = TestPass Seed
  | TestFail TestStage Seed TestSrc String
  | TestClangError Seed
  -- ^ For now, errors are treated the same as passes, since we're not
  -- concerned with clang bugs
  | TestExecTimeout Seed
  -- ^ If the Csmith-generated program fails to terminate quickly
  deriving (Eq, Show, Generic, NFData, Typeable)

instance X.Exception TestResult

data TestStage
  = DisasmStage
  | AsStage
  | ExecStage
  deriving (Eq, Ord, Show, Generic, NFData)

data TestSrc = TestSrc { srcFile :: FilePath, srcSize :: Integer }
  deriving (Eq, Show, Generic, NFData)

isPass :: TestResult -> Bool
isPass (TestPass _) = True
isPass _            = False

isFail :: TestResult -> Bool
isFail = not . isPass

runTest :: FilePath -> Clang -> Seed -> Options -> IO TestResult
runTest tmpDir (clangExe, includeDirs, flags) seed opts = X.handle return $ do
  let baseFile = show seed
      srcFile  = baseFile <.> "c"
      bcFile   = baseFile <.> "bc"
      llFile   = baseFile <.> "ll"
      llvmVersionFlags =
        case stripPrefix "clang-" clangExe of
          Nothing -> []
          Just ver -> [ "--llvm-version=" ++ ver ]
      includeOpts = concatMap (\dir -> ["-I", dir]) includeDirs
  putStrLn $ "Testing bitcode file " ++ bcFile
  ---- Run csmith ----
  csmithPath <- getCsmithPath opts
  callProcess "csmith" [
      "-o", tmpDir </> srcFile
    , "-s", show seed
    ]
  srcSize <- getFileSize (tmpDir </> srcFile)

  ---- Run clang ----
  h <- spawnProcess clangExe (words flags ++ [
      "-I" ++ csmithPath
    , "-c", "-emit-llvm"
    , tmpDir </> srcFile
    , "-o", tmpDir </> bcFile
    ] ++ includeOpts)
  clangErr <- waitForProcess h
  unless (clangErr == ExitSuccess) $ X.throw $!! TestClangError seed

  ---- Disassemble ----
  let disasmArgs = llvmVersionFlags ++ [ tmpDir </> bcFile ]
  (ec, out, err) <-
    readProcessWithExitCode "llvm-disasm" disasmArgs ""
  case ec of
    ExitFailure c -> do
      putStrLn "[DISASM ERROR]"
      putStrLn "[OUT]"
      putStr out
      putStrLn "[ERR]"
      putStr err
      putStrLn ("[ERROR CODE " ++ show c ++ "]")
      X.throw $!! TestFail DisasmStage seed TestSrc{..} err
    ExitSuccess -> return ()
  writeFile (tmpDir </> llFile) out

  ---- Assemble ----
  when (optRunAs opts) $ do
    (ec, out, err) <- readProcessWithExitCode clangExe (words flags ++ [
        "-I" ++ csmithPath
      , tmpDir </> llFile
      , "-o", tmpDir </> "ours"
      ] ++ includeOpts) ""
    case ec of
      ExitFailure c -> do
        putStrLn "[AS ERROR]"
        putStrLn "[OUT]"
        putStr out
        putStrLn "[ERR]"
        putStr err
        putStrLn ("[ERROR CODE " ++ show c ++ "]")
        X.throw $!! TestFail AsStage seed TestSrc{..} err
      ExitSuccess -> return ()

  ---- Re-run Clang ----
  when (optRunExec opts) $ do
    h <- spawnProcess clangExe (words flags ++ [
        "-I" ++ csmithPath
      , tmpDir </> bcFile
      , "-o", tmpDir </> "golden"
      ] ++ includeOpts)
    clangErr <- waitForProcess h
    unless (clangErr == ExitSuccess) $ X.throw $!! TestClangError seed
    timeout <- getTimeoutCmd
    (ec1, out1, err1) <- do
      readProcessWithExitCode timeout [ "10", (tmpDir </> "golden") ] ""
    unless (ec1 == ExitFailure 124) $ X.throw $!! TestExecTimeout seed
    (ec2, out2, err2) <-
      readProcessWithExitCode timeout [ "10", (tmpDir </> "ours") ] ""
    when (ec1 /= ec2 || out1 /= out2 || err1 /= err2) $
      X.throw $!! TestFail ExecStage seed TestSrc{..} err2

  ---- Succeed! ----
  putStrLn "[PASS]"
  return (TestPass seed)

getCsmithPath :: Options -> IO FilePath
getCsmithPath opts =
  case optCsmithPath opts of
    Just p -> return p
    Nothing -> do
      mp <- lookupEnv "CSMITH_PATH"
      case mp of
        Just p -> return p
        Nothing -> error "--csmith-path not given and CSMITH_PATH not set"

getTimeoutCmd :: IO FilePath
getTimeoutCmd = do
  mf <- findExecutable "timeout"
  case mf of
    Just timeout -> return timeout
    Nothing -> do
      mf <- findExecutable "gtimeout"
      case mf of
        Just timeout -> return timeout
        Nothing -> error "could not find `timeout' or `gtimeout' in PATH"

mkJUnitXml :: Map Clang [TestResult] -> IO Element
mkJUnitXml allResults = do
  hostname <- readProcess "hostname" [] ""
  now <- getZonedTime
  let nowFmt = formatTime
                 defaultTimeLocale
                 (iso8601DateFormat (Just "%H:%M:%S"))
                 now
      testsuites = map (testsuite hostname nowFmt) (Map.toList allResults)
  return $ unode "testsuites" testsuites
  where
    fmtClang (clangExe, _, flags) = toUnders ". " (clangExe ++ "_" ++ flags)
    testsuite hostname nowFmt (clang, results) =
      unode "testsuite" ([
          uattr "name"      "llvm-disasm fuzzer"
        , uattr "tests"     (show (length results))
        , uattr "failures"  (show (length (filter isFail results)))
        , uattr "errors"    "0"
        , uattr "skipped"   "0"
        , uattr "timestamp" nowFmt
        , uattr "time"      "0.0" -- irrelevant due to random input
        , uattr "id"        ""
        , uattr "package"   (fmtClang clang)
        , uattr "hostname"  hostname
        ]
        , flip map results $ \res ->
            case res of
              TestPass seed ->
                unode "testcase" [
                    uattr "name"      (show seed)
                  , uattr "classname" (fmtClang clang)
                  , uattr "time"      "0.0"
                  ]
              TestClangError seed ->
                unode "testcase" ([
                    uattr "name"      (show seed)
                  , uattr "classname" (fmtClang clang)
                  , uattr "time"      "0.0"
                  ]
                  , "clang error"
                  )
              TestExecTimeout seed ->
                unode "testcase" ([
                    uattr "name"      (show seed)
                  , uattr "classname" (fmtClang clang)
                  , uattr "time"      "0.0"
                  ]
                  , "Csmith-generated program did not terminate quickly"
                  )
              TestFail st seed _ err ->
                unode "testcase" ([
                    uattr "name"      (show seed ++ "_" ++ show st)
                  , uattr "classname" (fmtClang clang)
                  , uattr "time"      "0.0"
                  ]
                  , unode "failure" ([
                        uattr "message" ""
                      , uattr "type"    ""
                      ]
                      , err
                      )
                  )
        )

uattr :: String -> String -> Attr
uattr k v = Attr (unqual k) v

toUnders :: String -> String -> String
toUnders cs = map (\c -> if c `elem` cs then '_' else c)
