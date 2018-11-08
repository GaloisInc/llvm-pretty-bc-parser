-- * Regression tests

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Data.LLVM.BitCode (Error(..))

import           Control.Monad (when, forM, forM_, filterM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (nub)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Semigroup hiding ( Option )
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           System.Console.GetOpt (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import qualified System.Directory as Dir
import           System.Environment (getArgs, getProgName, getExecutablePath)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath (takeDirectory)
import qualified Turtle as T

import           Prelude


----------------------------------------------------------------
-- ** Option parsing

data Options = Options { optTests   :: [FilePath] -- ^ Tests
                       , optLlvmAs  :: Text       -- ^ llvm-as name
                       , optRev1    :: Text       -- ^ Git revision 1
                       , optRev2    :: Text       -- ^ Git revision 2
                       , optAST     :: Bool       -- ^ Compare generated ASTs?
                       , optHelp    :: Bool
                       } deriving (Eq, Ord, Show)

defaultOptions :: Options
defaultOptions  = Options { optTests   = ["disasm-test/tests/factorial2.ll"]
                          , optLlvmAs  = "llvm-as"
                          , optRev1    = "HEAD"
                          , optRev2    = "HEAD~1"
                          , optAST     = False
                          , optHelp    = False
                          }

options :: [OptDescr (Endo Options)]
options  =
  [ Option ""  ["llvm-as"] (ReqArg setLlvmAs "PATH") "path to/name of llvm-as"
  , Option ""  ["rev1"]    (ReqArg setRev1 "REV")    "first git revision to compare"
  , Option ""  ["rev2"]    (ReqArg setRev2 "REV")    "second git revision to compare"
  , Option ""  ["ast"]     (NoArg setAST)            "compare generated ASTs, rather than disassembled bitcode"
  , Option "h" ["help"]    (NoArg setHelp)           "display this message"
  ]
  where setLlvmAs str = Endo $ \opt -> opt { optLlvmAs = Text.pack str }
        setRev1   str = Endo $ \opt -> opt { optRev1   = Text.pack str }
        setRev2   str = Endo $ \opt -> opt { optRev2   = Text.pack str }
        setAST        = Endo $ \opt -> opt { optAST    = True          }
        setHelp       = Endo $ \opt -> opt { optHelp   = True          }

addTest :: String -> Endo Options
addTest test = Endo $ \opt -> opt { optTests = test : optTests opt }

getOptions :: IO Options
getOptions  =
  do args <- getArgs
     case getOpt (ReturnInOrder addTest) options args of

       (fs, [], []) -> let opts = appEndo (mconcat fs) defaultOptions
                       in if optHelp opts
                          then printUsage [] >> exitSuccess
                          else pure opts

       (_, _, errs) -> printUsage errs >> exitFailure

printUsage :: [String] -> IO ()
printUsage errs = do
  prog <- getProgName
  let banner = "Usage: " ++ prog ++ " [OPTIONS] test1.ll .. testn.ll"
  putStrLn (usageInfo (unlines (errs ++ [banner])) options)

  ----------------------------------------------------------------
-- ** Test running

------------------------------------------------------
-- *** Exceptions

-- | A test failure.
data TestFailure where
  -- | A parser failure. Occurs when the parser from one of the git revisions
  -- couldn't even parse the assembly.
  ParseError :: String -- ^ Which git revision?
             -> Error  -- ^ The parse error
             -> TestFailure
    deriving (Typeable, Eq, Ord, Show)

------------------------------------------------------
-- *** Outline

-- **** Preparing

--  1. Copy the entire source directory (located via the
--     `llvm-pretty-bc-parser.cabal` file) to a temporary "build" directory
--  2. Create a second temporary "output" directory
--  3. Copy all test `.ll` files to the "output" directory
--  4. Assemble the `.ll` files into `.bc` files with `llvm-as`
--  5. For each of the two specified git revisions,
--     i.   Check out that revision
--     ii.  Build llvm-disasm
--     iii. Copy the binary to llvm-disasm-<rev> in the output directory

-- **** Testing

--  6. Run llvm-disasm-<rev> on each `.bc` file, save the output as a file
--  7. Compare the two outputs, warn the user and print a diff if they aren't the
--     same

------------------------------------------------------
-- *** Running

-- | Beginning in the directory of the current executable, move upwards
-- and try to find `llvm-pretty-bc-parser.cabal`.
findSrc :: IO FilePath
findSrc = do
  parents       <- allParents . takeDirectory <$> getExecutablePath
  haveCabalFile <- flip filterM parents $
    fmap ("llvm-pretty-bc-parser.cabal" `elem` ) . Dir.listDirectory
  pure $ flip fromMaybe (listToMaybe haveCabalFile) $
    error $ unlines [ "Couldn't find cabal file in directories:"
                    , show parents
                    ]
  where -- This is quick-n-dirty: We assume the path has <200 components
        allParents = nub . take 200 . iterate takeDirectory

-- | Run all provided tests.
main :: IO ()
main = T.runManaged $ do
  opts      <- liftIO getOptions

  -- (1)
  src       <- liftIO $ T.decodeString <$> findSrc
  buildDir  <- T.mktempdir "/tmp" "regression-build"
  T.cptree src buildDir

  -- (2)
  outputDir <- T.mktempdir "/tmp" "regression-out"
  bcfiles   <- liftIO $ forM (optTests opts) $ \testFile -> do
    let llName = buildDir  T.</> T.decodeString testFile
    let bcName = llName    T.<.> "bc"

    -- (3)
    echoText $ "Assembling: " <> pathToText llName
    T.cp (T.decodeString testFile) llName

    -- (4)
    (code, stdout, stderr) <-
      T.procStrictWithErr (optLlvmAs opts)
        [ "-o"
        , pathToText bcName
        , pathToText llName
        ]
        (pure "")
    exitWithMsg ("Couldn't assemble " <> Text.pack testFile) code stdout stderr
    pure bcName

  let revs = [optRev1 opts, optRev2 opts]

  -- (5)
  T.cd buildDir
  liftIO $ forM_ revs $ \rev -> do
    echoText $ "Compiling: " <> rev

    -- (i)
    (code, stdout, stderr) <- T.procStrictWithErr "git" ["reset", "--hard", rev] (pure "")
    exitWithMsg ("Couldn't checkout rev " <> rev) code stdout stderr

    -- (ii)
    (code, stdout, stderr) <- T.procStrictWithErr "cabal" ["build"] (pure "")
    exitWithMsg ("Couldn't `cabal build` rev " <> rev) code stdout stderr

    -- (iii)
    T.cp (T.fromText "dist/build/llvm-disasm/llvm-disasm")
         (outputDir T.</> T.fromText ("llvm-disasm-" <> rev))

  -- (6)
  T.cd outputDir
  [a, b] <- liftIO $ forM revs $ \rev ->
    forM bcfiles $ \bcfile -> do
      let exe = pathToText outputDir <> "/" <> "llvm-disasm-" <> rev
      let ast = ["--ast" | optAST opts]
      let pat = pathToText bcfile
      (code, stdout, stderr) <-
        T.procStrictWithErr exe (ast ++ [pat]) (pure "")

      exitWithMsg ("Failed when disassembling " <> pat <> " with " <> exe)
        code stdout stderr

      let newPath = bcfile T.<.> rev T.<.> "ll"
      T.writeTextFile newPath stdout
      pure newPath

  -- (7)
  liftIO $ forM_ (zip a b) $ \(ll1, ll2) -> do
    let ll1t = pathToText ll1
        ll2t = pathToText ll2

    echoText $ "Diffing: " <> ll1t <> " " <> ll2t
    (code, stdout, stderr) <-
      T.procStrictWithErr "diff" [ll1t, ll2t] (pure "")
    exitWithMsg ("Failed when diffing " <> ll1t <> " with " <> ll2t)
      code stdout stderr

    mapM_ T.echo $ T.textToLines stdout

  where echoText     = liftIO . T.echo . T.unsafeTextToLine
        pathToText p =
          case T.toText p of
            Left err  -> error (show $ "Couldn't convert path to text: " <> err)
            Right str -> str
        exitWithMsg msg code stdout stderr =
          when (code /= T.ExitSuccess) $
            mapM_ (mapM_ T.echo . T.textToLines) [msg, stdout, stderr] >>
            exitFailure
