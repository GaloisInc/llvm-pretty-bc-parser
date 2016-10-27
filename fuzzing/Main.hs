{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
  (copyFile, createDirectoryIfMissing, getFileSize, getPermissions,
   setPermissions, setOwnerExecutable)
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

-- | The name of the @clang@ executable for a particular test
-- configuration, e.g., @clang-3.8@
type Clang = String

data Options = Options {
    optNumTests :: Integer
    -- ^ Number of Tests
  , optSeeds :: Maybe [Word64]
    -- ^ Specific seeds to use in fuzzing; overrides 'optNumTests'
  , optSaveTests :: Maybe FilePath
    -- ^ Location to save failed tests
  , optClangs :: [Clang]
    -- ^ Clangs to use with the fuzzer
  , optJUnitXml :: Maybe FilePath
    -- ^ Write JUnit test report
  , optCsmithPath :: Maybe FilePath
    -- ^ Path to Csmith include files
  , optCollapse :: Bool
    -- ^ Whether to collapse failures with the same error message
  , optReduce :: Bool
    -- ^ Whether to try reducing the test case with Creduce
  , optHelp :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optNumTests   = 100
  , optSeeds      = Nothing
  , optSaveTests  = Nothing
  , optClangs     = ["clang"]
  , optJUnitXml   = Nothing
  , optCsmithPath = Nothing
  , optCollapse   = False
  , optReduce     = False
  , optHelp       = False
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
  , Option ""  ["junit-xml"] (ReqArg setJUnitXml "FILEPATH")
    "output JUnit-style XML test report"
  , Option ""  ["csmith-path"] (ReqArg setCsmithPath "DIRECTORY")
    "path to Csmith include files; default is $CSMITH_PATH environment variable"
  , Option ""  ["collapse"] (NoArg setCollapse)
    "collapse failing test cases by error message and remove successes"
  , Option ""  ["reduce"] (NoArg setReduce)
    "reduce failing test cases with a best-guess Creduce (requires `--output`)"
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
  case optClangs opt of
    ["clang"] -> opt { optClangs = [str] }
    clangs    -> opt { optClangs = str:clangs }

setJUnitXml :: String -> Endo Options
setJUnitXml str = Endo (\opt -> opt { optJUnitXml = Just str })

setCsmithPath :: String -> Endo Options
setCsmithPath str = Endo (\opt -> opt { optCsmithPath = Just str })

setCollapse :: Endo Options
setCollapse = Endo (\opt -> opt { optCollapse = True })

setReduce :: Endo Options
setReduce = Endo (\opt -> opt { optReduce = True })

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
  when (optSaveTests opts == Nothing && optReduce opts) $
    printUsage [ "--reduce requires --output to be set" ]
  -- run the tests within each clang version in parallel. We could
  -- parallelize the runs across clang versions as well, but it's
  -- probably not worth the complexity at that level of granularity
  resultMaps <-
    forM (optClangs opts) $ \clang -> runParIO $ do
      liftIO $ putStrLn $ "[" ++ clang ++ "]"
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
  let allResults' = Map.unions resultMaps
      allResults | optCollapse opts = collapseResults allResults'
                 | otherwise        = allResults'
  forM_ (Map.toList allResults) $ \(clang, results) -> do
    let (_passes, fails) = partition isPass results
    when (not (null fails)) $ do
      putStrLn $ "[" ++ clang ++ "] " ++
        show (length fails) ++ " failing cases identified:"
      forM_ fails $ \case
        TestFail st s _ _ -> putStrLn ("[" ++ show st ++ "] " ++ show s)
        _ -> error "non-fail cases in fails"
  case optSaveTests opts of
    Nothing -> return ()
    Just root -> do
      createDirectoryIfMissing False root
      forM_ (Map.toList allResults) $ \(clang, results) ->
        when (not (null (filter isFail results))) $ do
          let clangRoot = root </> clang
          createDirectoryIfMissing False clangRoot
          forM_ results $ \result ->
            case result of
              TestPass _ -> return ()
              -- errors arise from bugs in clang, not our code
              TestClangError _ -> return ()
              TestFail st _ TestSrc{..} err -> do
                copyFile (tmpDir </> srcFile) (clangRoot </> srcFile)
                writeFile (clangRoot </> srcFile <.> show st <.> "err") err
                when (optReduce opts) $
                  reduce result clang opts clangRoot
  case optJUnitXml opts of
    Nothing -> return ()
    Just f -> do
      xml <- mkJUnitXml allResults
      writeFile f (ppTopElement xml)

reduce :: TestResult -> Clang -> Options -> FilePath -> IO ()
reduce (TestFail st _ TestSrc{..} err) clang opts clangRoot = do
  csmithPath <- getCsmithPath opts
  -- copy a file for the reduction in place
  let baseName   = dropExtension srcFile
      srcReduced = clangRoot </> baseName ++ "-reduced.c"
      scriptFile = clangRoot </> baseName ++ "-reduce.sh"
      llvmVersion =
        case stripPrefix "clang-" clang of
          Nothing -> ""
          Just ver -> "--llvm-version=" ++ ver
  copyFile (clangRoot </> srcFile) srcReduced
  let grepPat DisasmStage =
        -- look for the first line of the error starting with a
        -- tab; this should be the top of the llvm-disasm stack
        -- trace
        case dropWhile (not . isPrefixOf "\t") (lines err) of
          -- if we don't have a stack trace, we probably shouldn't
          -- waste our time
          [] -> Nothing
          first:_ -> Just first
      grepPat AsStage =
        -- check the first line of the clang error for "error:"
        case (lines err) of
          [] -> Nothing
          first:_ ->
            case dropWhile (/= "error:") (words first) of
              [] -> Nothing
              msg -> Just (unwords msg)
      grepPat ExecStage = error "no grep pattern for exec reduction"
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
      buildBc = unwords [
          clang, "-I", csmithPath, "-O -g -w -c"
        , "-emit-llvm", baseName ++ "-reduced.c", "-o", baseName <.> "bc"
        ]
      buildLl = unwords [
          "llvm-disasm", llvmVersion, baseName <.> "bc", ">", baseName <.> "ll"
        ]
      script DisasmStage = unlines $ scriptHeader ++ [
          buildBc
        , unwords [ "llvm-disasm", llvmVersion, baseName <.> "bc", "2>&1 |"
                  , "fgrep", show (fromMaybe "" (grepPat st))
                  ]
        ]
      script AsStage = unlines $ scriptHeader ++ [
          buildBc
        , buildLl
        , unwords [ clang, "-I", csmithPath, "-O -g -w -c"
                  , baseName <.> "ll", "-o", baseName <.> "o", "2>&1 |"
                  , "fgrep ", show (fromMaybe "" (grepPat st))
                  ]
        ]
      script ExecStage = unlines $ scriptHeader ++ [
          buildBc
        , buildLl
        , unwords [ clang, "-I", csmithPath, "-O -g -w"
                  , baseName <.> "bc", "-o", "golden"
                  ]
        , unwords [ clang, "-I", csmithPath, "-O -g -w"
                  , baseName <.> "ll", "-o", "ours"
                  ]
        , "[ \"$(./golden)\" != \"$(./ours)\" ]"
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
runTest tmpDir clang seed opts = X.handle return $ do
  let baseFile = clang ++ "-" ++ show seed
      srcFile  = baseFile <.> "c"
      bcFile   = baseFile <.> "bc"
      llFile   = baseFile <.> "ll"
      llvmVersion =
        case stripPrefix "clang-" clang of
          Nothing -> ""
          Just ver -> "--llvm-version=" ++ ver
  csmithPath <- getCsmithPath opts
  callProcess "csmith" [
      "-o", tmpDir </> srcFile
    , "-s", show seed
    ]
  srcSize <- getFileSize (tmpDir </> srcFile)
  h <- spawnProcess clang [
      "-I" ++ csmithPath
    , "-O", "-g", "-w", "-c", "-emit-llvm"
    , tmpDir </> srcFile
    , "-o", tmpDir </> bcFile
    ]
  clangErr <- waitForProcess h
  unless (clangErr == ExitSuccess) $ X.throw $!! TestClangError seed
  (ec, out, err) <-
    readProcessWithExitCode "llvm-disasm" [ llvmVersion, tmpDir </> bcFile ] ""
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
  (ec, out, err) <- readProcessWithExitCode clang [
      "-I" ++ csmithPath
    , "-O", "-g", "-w"
    , tmpDir </> llFile
    , "-o", tmpDir </> "ours"
    ] ""
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
  h <- spawnProcess clang [
      "-I" ++ csmithPath
    , "-O", "-g", "-w"
    , tmpDir </> bcFile
    , "-o", tmpDir </> "golden"
    ]
  clangErr <- waitForProcess h
  unless (clangErr == ExitSuccess) $ X.throw $!! TestClangError seed
  (ec1, out1, err1) <-
    readProcessWithExitCode (tmpDir </> "golden") [] ""
  (ec2, out2, err2) <-
    readProcessWithExitCode (tmpDir </> "ours") [] ""
  when (ec1 /= ec2 || out1 /= out2 || err1 /= err2) $
    X.throw $!! TestFail ExecStage seed TestSrc{..} err2
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
    toUnder = map (\c -> if c == '.' then '_' else c)
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
        , uattr "package"   (toUnder clang)
        , uattr "hostname"  hostname
        ]
        , flip map results $ \res ->
            case res of
              TestPass seed ->
                unode "testcase" [
                    uattr "name"      (show seed)
                  , uattr "classname" (toUnder clang)
                  , uattr "time"      "0.0"
                  ]
              TestClangError seed ->
                unode "testcase" ([
                    uattr "name"      (show seed)
                  , uattr "classname" (toUnder clang)
                  , uattr "time"      "0.0"
                  ]
                  , "clang error"
                  )
              TestFail st seed _ err ->
                unode "testcase" ([
                    uattr "name"      (show seed ++ "_" ++ show st)
                  , uattr "classname" (toUnder clang)
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
