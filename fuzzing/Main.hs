module Main where

import Control.Monad-- (forM_, when)
import Data.List (partition)
--import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat, Endo(..))
import Data.Time
import Data.Word (Word64)
import System.Console.GetOpt
           (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withTempDirectory)
import System.Process
import System.Random (randomIO)
import Text.Read (readMaybe)
import Text.XML.Light

-- Option Parsing --------------------------------------------------------------

data Options = Options {
    optNumTests :: Integer
    -- ^ Number of Tests
  , optSaveTests :: Maybe FilePath
    -- ^ Location to save failed tests
  , optJUnitXml :: Maybe FilePath
    -- ^ Write JUnit test report
  , optCsmithPath :: Maybe FilePath
    -- ^ Path to Csmith include files
  , optHelp    :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optNumTests   = 100
  , optSaveTests  = Nothing
  , optJUnitXml   = Nothing
  , optCsmithPath = Nothing
  , optHelp       = False
  }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "n" [] (ReqArg setNumTests "NUMBER")
    "number of tests to run"
  , Option ""  ["save"] (ReqArg setSaveTests "DIRECTORY")
    "directory to save failed tests"
  , Option ""  ["junit-xml"] (ReqArg setJUnitXml "FILEPATH")
    "output JUnit-style XML test report"
  , Option ""  ["csmith-path"] (ReqArg setCsmithPath "DIRECTORY")
    "path to Csmith include files; default is $CSMITH_PATH environment variable"
  , Option "h" ["help"] (NoArg setHelp)
    "display this message"
  ]

setNumTests :: String -> Endo Options
setNumTests str = Endo $ \opt ->
  case readMaybe str of
    Just n -> opt { optNumTests = n }
    Nothing -> error "expected integer number of tests"

setSaveTests :: String -> Endo Options
setSaveTests str = Endo (\opt -> opt { optSaveTests = Just str })

setJUnitXml :: String -> Endo Options
setJUnitXml str = Endo (\opt -> opt { optJUnitXml = Just str })

setCsmithPath :: String -> Endo Options
setCsmithPath str = Endo (\opt -> opt { optCsmithPath = Just str })

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
main = do
  opts <- getOptions
  hostname <- readProcess "hostname" [] ""
  now <- getZonedTime
  results <- forM [1..optNumTests opts] $ \_ -> do
    seed <- randomIO
    runTest seed opts
  let (_passes, fails) = partition isPass results
  when (not (null fails)) $ do
    putStrLn $
      show (length fails) ++ "/" ++ show (optNumTests opts) ++ " tests failed."
    putStrLn "Failed test seeds:"
    forM_ fails $ \(TestFail s _ _) ->
      print s
  case optJUnitXml opts of
    Nothing -> return ()
    Just f -> writeFile f (ppTopElement (mkJUnitXml hostname now results))

type Seed = Word64

data TestResult
  = TestPass Seed
  | TestFail Seed FilePath String
  deriving (Eq, Show)

isPass :: TestResult -> Bool
isPass (TestPass _) = True
isPass _            = False

isFail :: TestResult -> Bool
isFail = not . isPass

runTest :: Seed -> Options -> IO TestResult
runTest seed opts = withTempDirectory "." ".fuzz." $ \tmpDir -> do
  let testSrc = tmpDir </> "test-" ++ show seed <.> "c"
  csmithPath <- getCsmithPath opts
  callProcess "csmith" [
      "-o", tmpDir </> "test.c"
    , "-s", show seed
    ]
  callProcess "clang" [
      "-I" ++ csmithPath
    , "-O", "-g", "-w", "-c", "-emit-llvm"
    , tmpDir </> "test.c"
    , "-o", tmpDir </> "test.bc"
    ]
  (ec, out, err) <-
    readProcessWithExitCode "llvm-disasm" [ tmpDir </> "test.bc" ] ""
  putStrLn "[OUT]"
  putStr out
  putStrLn "[ERR]"
  putStr err
  case ec of
    ExitSuccess -> return (TestPass seed)
    ExitFailure c -> do
      putStrLn ("[ERROR CODE " ++ show c ++ "]")
      return (TestFail seed testSrc err)

getCsmithPath :: Options -> IO FilePath
getCsmithPath opts =
  case optCsmithPath opts of
    Just p -> return p
    Nothing -> do
      mp <- lookupEnv "CSMITH_PATH"
      case mp of
        Just p -> return p
        Nothing -> error "--csmith-path not given and CSMITH_PATH not set"

mkJUnitXml :: String -> ZonedTime -> [TestResult] -> Element
mkJUnitXml hostname now results =
  unode "testsuites" $
  unode "testsuite" ([
      uattr "name"      "llvm-disasm fuzzer"
    , uattr "tests"     (show (length results))
    , uattr "failures"  (show (length fails))
    , uattr "errors"    "0"
    , uattr "skipped"   "0"
    , uattr "timestamp" nowFmt
    , uattr "time"      "0.0" -- irrelevant due to random input
    , uattr "id"        ""
    , uattr "package"   ""
    , uattr "hostname"  hostname
    ]
    , flip map results $ \res ->
        case res of
          TestPass seed ->
            unode "testcase" [
                uattr "name"      (show seed)
              , uattr "classname" ""
              , uattr "time"      "0.0"
              ]
          TestFail seed _ err ->
            unode "testcase" ([
                uattr "name"      (show seed)
              , uattr "classname" ""
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
  where
  (_passes, fails) = partition isPass results
  nowFmt =
    formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) now

uattr :: String -> String -> Attr
uattr k v = Attr (unqual k) v
