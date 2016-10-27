{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.DeepSeq (($!!), NFData)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Class (get, spawn)
import Control.Monad.Par.IO (runParIO)
import Data.List (isPrefixOf, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat, Endo(..))
import Data.Time
  (defaultTimeLocale, formatTime, getZonedTime, iso8601DateFormat)
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
        TestDisasmFail s _ _ -> putStrLn ("[disasm] " ++ show s)
        TestAsFail     s _ _ -> putStrLn ("[as] " ++ show s)
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
              TestDisasmFail _ TestSrc{..} err -> do
                copyFile (tmpDir </> srcFile) (clangRoot </> srcFile)
                writeFile (clangRoot </> srcFile <.> "disasm_err") err
                when (optReduce opts) $
                  reduceDisasm clang opts clangRoot srcFile err
              TestAsFail _ TestSrc{..} err -> do
                copyFile (tmpDir </> srcFile) (clangRoot </> srcFile)
                writeFile (clangRoot </> srcFile <.> "as_err") err
                when (optReduce opts) $
                  reduceAs clang opts clangRoot srcFile err
  case optJUnitXml opts of
    Nothing -> return ()
    Just f -> do
      xml <- mkJUnitXml allResults
      writeFile f (ppTopElement xml)

-- | Best-effort reduction of disassembler test cases using
-- Creduce. Much of the art of using Creduce is in writing a correct
-- interestingness test, which we try to approximate here using the
-- stack trace from @llvm-disasm@. We also do not introduce any
-- parallelism here because Creduce introduces its own parallelism
-- when we invoke it.
reduceDisasm :: Clang -> Options -> FilePath -> FilePath -> String -> IO ()
reduceDisasm clang opts clangRoot srcFile err = do
  csmithPath <- getCsmithPath opts
  -- copy a file for the reduction in place
  let baseName   = dropExtension srcFile
      srcReduced = clangRoot </> baseName ++ "-reduced.c"
      scriptFile = clangRoot </> baseName ++ "-reduce.sh"
  copyFile (clangRoot </> srcFile) srcReduced
  -- find the best guess at the grep pattern for the Creduce script by
  -- looking for the first line starting with a tab; this should be
  -- the top of the stack trace
  let grepPat =
        case dropWhile (not . isPrefixOf "\t") (lines err) of
          -- if we don't have a stack trace, we probably shouldn't
          -- waste our time
          [] -> Nothing
          first:_ -> Just first
      script = unlines [
          "#!/usr/bin/env bash"
        , "# Consider this script a best guess template for reducing failing"
        , "# test cases. Not every bug will manifest in the same way and give"
        , "# the same error message, so modify the grep condition appropriately"
        , "# if the shrink is unsatisfactory."
        , ""
        , concat [ clang, " -I", csmithPath, " -O -g -w -c "
                 , "-emit-llvm ", baseName ++"-reduced.c", " -o ", baseName <.> "bc" ]
        , "if [ $? -ne 0 ]; then"
        , "    exit 1"
        , "fi"
        , concat [ "llvm-disasm ", baseName <.> "bc", " 2>&1 | "
                 , "fgrep ", show (fromMaybe "" grepPat)]
        ]
  when (grepPat /= Nothing) $ do
    -- write out the shell script to drive Creduce and run it
    writeFile scriptFile script
    p <- getPermissions scriptFile
    setPermissions scriptFile (setOwnerExecutable True p)
    h <- spawnProcess "creduce" [ scriptFile, srcReduced ]
    void $ waitForProcess h

-- | Best-effort reduction of assembler (roundtrip) test cases using
-- Creduce. Much of the art of using Creduce is in writing a correct
-- interestingness test, which we try to approximate here using the
-- error text from @clang@. We also do not introduce any parallelism
-- here because Creduce introduces its own parallelism when we invoke
-- it.
reduceAs :: Clang -> Options -> FilePath -> FilePath -> String -> IO ()
reduceAs clang opts clangRoot srcFile err = do
  csmithPath <- getCsmithPath opts
  -- copy a file for the reduction in place
  let baseName   = dropExtension srcFile
      srcReduced = clangRoot </> baseName ++ "-reduced.c"
      scriptFile = clangRoot </> baseName ++ "-reduce.sh"
  copyFile (clangRoot </> srcFile) srcReduced
  -- find the best guess at the grep pattern for the Creduce script by
  -- checking the first line of stderr output for "error:"
  let grepPat =
        case (lines err) of
          [] -> Nothing
          first:_ ->
            case dropWhile (/= "error:") (words first) of
              [] -> Nothing
              msg -> Just (unwords msg)
      script = unlines [
          "#!/usr/bin/env bash"
        , "# Consider this script a best guess template for reducing failing"
        , "# test cases. Not every bug will manifest in the same way and give"
        , "# the same error message, so modify the grep condition appropriately"
        , "# if the shrink is unsatisfactory."
        , ""
        , concat [ clang, " -I", csmithPath, " -O -g -w -c "
                 , "-emit-llvm ", baseName ++"-reduced.c", " -o ", baseName <.> "bc" ]
        , "if [ $? -ne 0 ]; then"
        , "    exit 1"
        , "fi"
        , concat [ "llvm-disasm ", baseName <.> "bc", " > ", baseName <.> "ll" ]
        , "if [ $? -ne 0 ]; then"
        , "    exit 1"
        , "fi"
        , concat [ clang, " -I", csmithPath, " -O -g -w -c "
                 , baseName <.> "ll", " -o ", baseName <.> "o", " 2>&1 | "
                 , "fgrep ", show (fromMaybe "" grepPat)]
        ]
  when (grepPat /= Nothing) $ do
    -- write out the shell script to drive Creduce and run it
    writeFile scriptFile script
    p <- getPermissions scriptFile
    setPermissions scriptFile (setOwnerExecutable True p)
    h <- spawnProcess "creduce" [ scriptFile, srcReduced ]
    void $ waitForProcess h

collapseResults :: Map Clang [TestResult] -> Map Clang [TestResult]
collapseResults = Map.map (collapse (Map.empty, Map.empty))
  where
    collapse (disasm, as) [] = Map.elems disasm ++ Map.elems as
    collapse (disasm, as) (r:results) =
      case r of
        TestPass _ -> collapse (disasm, as) results
        -- errors arise from bugs in clang, not our code
        TestClangError _ -> collapse (disasm, as) results
        TestDisasmFail _ _ err ->
          collapse ((Map.insertWith f err r disasm), as) results
        TestAsFail _ _ err ->
          collapse (disasm, (Map.insertWith f err r as)) results
    -- choose the smallest source file
    f r1@(TestDisasmFail _ src1 _) r2@(TestDisasmFail _ src2 _) =
      case compare (srcSize src1) (srcSize src2) of
        LT -> r1
        EQ -> r1 -- arbitrarily
        GT -> r2
    f r1@(TestAsFail _ src1 _) r2@(TestAsFail _ src2 _) =
      case compare (srcSize src1) (srcSize src2) of
        LT -> r1
        EQ -> r1 -- arbitrarily
        GT -> r2
    f _ _ = error "only test failures should go in this map"

type Seed = Word64

data TestResult
  = TestPass Seed
  | TestDisasmFail Seed TestSrc String
  | TestAsFail Seed TestSrc String
  | TestClangError Seed
  -- ^ For now, errors are treated the same as passes, since we're not
  -- concerned with clang bugs
  deriving (Eq, Show, Generic, NFData)

data TestSrc = TestSrc { srcFile :: FilePath, srcSize :: Integer }
  deriving (Eq, Show, Generic, NFData)

isPass :: TestResult -> Bool
isPass (TestPass _) = True
isPass _            = False

isFail :: TestResult -> Bool
isFail = not . isPass

runTest :: FilePath -> Clang -> Seed -> Options -> IO TestResult
runTest tmpDir clang seed opts = do
  let baseFile = clang ++ "-" ++ show seed
      srcFile  = baseFile <.> "c"
      bcFile   = baseFile <.> "bc"
      llFile   = baseFile <.> "ll"
  csmithPath <- getCsmithPath opts
  callProcess "csmith" [
      "-o", tmpDir </> srcFile
    , "-s", show seed
    ]
  h <- spawnProcess clang [
      "-I" ++ csmithPath
    , "-O", "-g", "-w", "-c", "-emit-llvm"
    , tmpDir </> srcFile
    , "-o", tmpDir </> bcFile
    ]
  clangErr <- waitForProcess h
  case clangErr of
    ExitFailure _ -> return (TestClangError seed)
    _ -> do
      (ec, out, err) <-
        readProcessWithExitCode "llvm-disasm" [ tmpDir </> bcFile ] ""
      case ec of
        ExitFailure c -> do
          putStrLn "[DISASM ERROR]"
          putStrLn "[OUT]"
          putStr out
          putStrLn "[ERR]"
          putStr err
          putStrLn ("[ERROR CODE " ++ show c ++ "]")
          srcSize <- getFileSize (tmpDir </> srcFile)
          return $!! TestDisasmFail seed TestSrc{..} err
        ExitSuccess -> do
          writeFile (tmpDir </> llFile) out
          (ec', out', err') <- readProcessWithExitCode clang [
              "-I" ++ csmithPath
            , "-O", "-g", "-w", "-c"
            , tmpDir </> llFile
            , "-o", tmpDir </> baseFile <.> "o"
            ] ""
          case ec' of
            ExitFailure c -> do
              putStrLn "[AS ERROR]"
              putStrLn "[OUT]"
              putStr out'
              putStrLn "[ERR]"
              putStr err'
              putStrLn ("[ERROR CODE " ++ show c ++ "]")
              srcSize <- getFileSize (tmpDir </> srcFile)
              return $!! TestAsFail seed TestSrc{..} err'
            ExitSuccess -> do
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
              TestDisasmFail seed _ err ->
                unode "testcase" ([
                    uattr "name"      (show seed ++ "_disasm")
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
              TestAsFail seed _ err ->
                unode "testcase" ([
                    uattr "name"      (show seed ++ "_as")
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
