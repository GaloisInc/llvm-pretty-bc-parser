{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.DeepSeq (($!!), NFData)
import Control.Monad-- (forM_, when)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat, Endo(..))
import Data.Time
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Console.GetOpt
           (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import System.Directory (copyFile, createDirectory, getFileSize, removeDirectoryRecursive)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withTempDirectory)
import System.Process
import System.Random (randomIO)
import Text.Read (readMaybe)
import Text.XML.Light

-- Option Parsing --------------------------------------------------------------

-- | The name of the @clang@ executable for a particular test
-- configuration, e.g., @clang-3.8@
type Clang = String

data Options = Options {
    optNumTests :: Integer
    -- ^ Number of Tests
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
  , optHelp    :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optNumTests   = 100
  , optSaveTests  = Nothing
  , optClangs     = ["clang"]
  , optJUnitXml   = Nothing
  , optCsmithPath = Nothing
  , optCollapse   = False
  , optHelp       = False
  }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "n" [] (ReqArg setNumTests "NUMBER")
    "number of tests to run"
  , Option "o"  ["output"] (ReqArg setSaveTests "DIRECTORY")
    "directory to save failed tests"
  , Option "c" ["clang"] (ReqArg addClang "CLANG")
    "specify clang executables to use, e.g., `-c clang-3.8 -c clang-3.9'"
  , Option ""  ["junit-xml"] (ReqArg setJUnitXml "FILEPATH")
    "output JUnit-style XML test report"
  , Option ""  ["csmith-path"] (ReqArg setCsmithPath "DIRECTORY")
    "path to Csmith include files; default is $CSMITH_PATH environment variable"
  , Option ""  ["collapse-results"] (NoArg setCollapse)
    "collapse failing test cases by error message and remove successes"
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
  resultMaps <-
    forM (optClangs opts) $ \clang -> do
      putStrLn $ "[" ++ clang ++ "]"
      results <- forM [1..optNumTests opts] $ \_ -> do
        seed <- randomIO
        runTest tmpDir clang seed opts
      return (Map.singleton clang results)
  let allResults' = Map.unions resultMaps
      allResults | optCollapse opts = collapseResults allResults'
                 | otherwise        = allResults'
  forM_ (Map.toList allResults) $ \(clang, results) -> do
    let (_passes, fails) = partition isPass results
    when (not (null fails)) $ do
      putStrLn $ "[" ++ clang ++ "] " ++
        show (length fails) ++ " failing cases identified:"
      forM_ fails $ \(TestFail s _ _) ->
        print s
  case optSaveTests opts of
    Nothing -> return ()
    Just root -> do
      removeDirectoryRecursive root
      createDirectory root
      forM_ (Map.toList allResults) $ \(clang, results) ->
        when (not (null (filter isFail results))) $ do
          let clangRoot = root </> clang
          createDirectory clangRoot
          forM_ results $ \result ->
            case result of
              TestPass _ -> return ()
              TestFail _ TestSrc{..} err -> do
                copyFile (tmpDir </> srcFile) (clangRoot </> srcFile)
                writeFile (clangRoot </> srcFile <.> "stderr") err
  case optJUnitXml opts of
    Nothing -> return ()
    Just f -> do
      xml <- mkJUnitXml allResults
      writeFile f (ppTopElement xml)

collapseResults :: Map Clang [TestResult] -> Map Clang [TestResult]
collapseResults = Map.map (collapse Map.empty)
  where
    collapse seen [] = Map.elems seen
    collapse seen (r:results) =
      case r of
        TestPass _ -> collapse seen results
        TestFail _ _ err ->
          collapse (Map.insertWith f err r seen) results
    -- choose the smallest source file
    f r1@(TestFail _ src1 _) r2@(TestFail _ src2 _) =
      case compare (srcSize src1) (srcSize src2) of
        LT -> r1
        EQ -> r1 -- arbitrarily
        GT -> r2
    f _ _ = error "only TestFails should go in this map"

type Seed = Word64

data TestResult
  = TestPass Seed
  | TestFail Seed TestSrc String
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
  csmithPath <- getCsmithPath opts
  callProcess "csmith" [
      "-o", tmpDir </> srcFile
    , "-s", show seed
    ]
  callProcess clang [
      "-I" ++ csmithPath
    , "-O", "-g", "-w", "-c", "-emit-llvm"
    , tmpDir </> srcFile
    , "-o", tmpDir </> bcFile
    ]
  (ec, out, err) <-
    readProcessWithExitCode "llvm-disasm" [ tmpDir </> bcFile ] ""
  case ec of
    ExitSuccess -> do
      putStrLn "[PASS]"
      return (TestPass seed)
    ExitFailure c -> do
      putStrLn "[ERROR]"
      putStrLn "[OUT]"
      putStr out
      putStrLn "[ERR]"
      putStr err
      putStrLn ("[ERROR CODE " ++ show c ++ "]")
      srcSize <- getFileSize (tmpDir </> srcFile)
      return $!! TestFail seed TestSrc{..} err

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
              TestFail seed _ err ->
                unode "testcase" ([
                    uattr "name"      (show seed)
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
