{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.LLVM.BitCode (parseBitCodeLazyFromFile,Error(..),formatError)
import qualified Text.LLVM.AST as AST
import           Text.LLVM.PP (ppLLVM,ppModule)

import qualified Control.Exception as X
import           Control.Lens ((^.), (^?), _Right, to)
import           Control.Monad (guard, unless, when)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as L
import           Data.Char (ord,isLetter,isSpace,chr)
import           Data.Generics (everywhere, mkT) -- SYB
import           Data.List (isInfixOf, isPrefixOf, sort, stripPrefix)
import           Data.Maybe (mapMaybe)
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Versions (Versioning, versioning, prettyV, major)
import qualified GHC.IO.Exception as GE
import qualified Options.Applicative as OA
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Exit (ExitCode(..), exitFailure, exitSuccess)
import           System.FilePath ((<.>), takeFileName)
import qualified System.IO as IO (stderr)
import           System.IO
    (openBinaryTempFile,hClose,openTempFile,hPutStrLn)
import qualified System.Process as Proc
import           Test.Tasty
import           Test.Tasty.HUnit ( assertFailure, testCase )
import qualified Test.Tasty.Options as TO
import qualified Test.Tasty.Runners as TR
import qualified Test.Tasty.Sugar as TS
import           Text.Read (readMaybe)
import           Text.Show.Pretty (ppShow)


-- Option Parsing --------------------------------------------------------------

newtype LLVMAs = LLVMAs FilePath

instance TO.IsOption LLVMAs where
  defaultValue = LLVMAs "llvm-as"
  parseValue = Just . LLVMAs
  optionName = pure "with-llvm-as"
  optionHelp = pure "path to llvm-as"
  showDefaultValue (LLVMAs as) = Just as
  optionCLParser = TO.mkOptionCLParser $
    OA.metavar "FILEPATH"

newtype LLVMDis = LLVMDis FilePath

instance TO.IsOption LLVMDis where
  defaultValue = LLVMDis "llvm-dis"
  parseValue = Just . LLVMDis
  optionName = pure "with-llvm-dis"
  optionHelp = pure "path to llvm-dis"
  showDefaultValue (LLVMDis dis) = Just dis
  optionCLParser = TO.mkOptionCLParser $
    OA.metavar "FILEPATH"

newtype Roundtrip = Roundtrip Bool

instance TO.IsOption Roundtrip where
  defaultValue = Roundtrip True
  parseValue = fmap Roundtrip . TO.safeReadBool
  optionName = pure "roundtrip"
  optionHelp = pure "disable roundtrip tests (AST/AST diff)"
  showDefaultValue (Roundtrip r) = Just $ show r
  optionCLParser = TO.mkOptionCLParser $
    OA.short 'r'

newtype Keep = Keep Bool

instance TO.IsOption Keep where
  defaultValue = Keep False
  parseValue = fmap Keep . TO.safeReadBool
  optionName = pure "keep"
  optionHelp = pure "keep all generated files for manual inspection"
  showDefaultValue (Keep k) = Just $ show k
  optionCLParser = TO.mkOptionCLParser $
    OA.short 'k'

disasmTestIngredients :: [TR.Ingredient]
disasmTestIngredients =
  includingOptions [ TO.Option (Proxy @LLVMAs)
                   , TO.Option (Proxy @LLVMDis)
                   , TO.Option (Proxy @Roundtrip)
                   , TO.Option (Proxy @Keep)
                   ] :
  defaultIngredients

parseCmdLine :: IO TO.OptionSet
parseCmdLine = do
  TR.installSignalHandlers
  let disasmOptDescrs = TO.uniqueOptionDescriptions $
        TR.coreOptions ++
        TS.sugarOptions ++
        TR.ingredientsOptions disasmTestIngredients
      (disasmOptWarns, disasmOptParser) = TR.optionParser disasmOptDescrs
  mapM_ (hPutStrLn IO.stderr) disasmOptWarns
  OA.execParser $
    OA.info (OA.helper <*> disasmOptParser)
    ( OA.fullDesc <>
      OA.header "llvm-pretty-bc-parser disassembly test suite"
    )


-- Querying Tool Versions ------------------------------------------------------

-- | Captures the name of the tool and either the error when attempting to get
-- the tool version or the actual parsed version self-reported by the tool.  Lack
-- of a decipherable version is not fatal to running the tests.
data VersionCheck = VC String (Either T.Text Versioning)

showVC :: VersionCheck -> String
showVC (VC nm v) = nm <> " " <> (T.unpack $ either id prettyV v)

vcTag :: VersionCheck -> String
vcTag v@(VC nm _) = nm <> vcMajor v

vcMajor :: VersionCheck -> String
vcMajor (VC _ v) = either T.unpack (^. major . to show) v

vcVersioning :: VersionCheck -> Either T.Text Versioning
vcVersioning (VC _ v) = v

mkVC :: String -> String -> VersionCheck
mkVC nm raw = let r = T.pack raw in VC nm $ first (const r) $ versioning r

-- Check if a VersionCheck version is less than the numeric value of another
-- version (represented as a Word).
vcLT :: VersionCheck -> Word -> Bool
vcLT vc verNum = (vcVersioning vc ^? (_Right . major)) < Just verNum

-- Check if a VersionCheck version is greater than or equal to the numeric
-- value of another version (represented as a Word).
vcGE :: VersionCheck -> Word -> Bool
vcGE vc verNum = (vcVersioning vc ^? (_Right . major)) >= Just verNum

getLLVMAsVersion :: LLVMAs -> IO VersionCheck
getLLVMAsVersion (LLVMAs llvmAsPath) = getLLVMToolVersion "llvm-as" llvmAsPath

getLLVMDisVersion :: LLVMDis -> IO VersionCheck
getLLVMDisVersion (LLVMDis llvmDisPath) = getLLVMToolVersion "llvm-dis" llvmDisPath

-- Determine which version of an LLVM tool will be used for these tests (if
-- possible).  Uses partial 'head' but this is just tests, and failure is
-- captured.
getLLVMToolVersion :: String -> FilePath -> IO VersionCheck
getLLVMToolVersion toolName toolPath = do
  let isVerLine = isInfixOf "LLVM version"
      dropLetter = dropWhile (all isLetter)
      getVer (Right inp) =
        -- example inp: "LLVM version 10.0.1"
        head $ dropLetter $ words $ head $ filter isVerLine $ lines inp
      getVer (Left full) = full
  mkVC toolName . getVer <$> readProcessVersion toolPath

-- Runs the tool with a --version argument to have it self-report its version.
-- The tool may not even be installed.  Returns either an error string or the
-- output string from the tool.
readProcessVersion :: String -> IO (Either String String)
readProcessVersion forTool =
  X.catches (Right <$> Proc.readProcess forTool [ "--version" ] "")
  [ X.Handler $ \(e :: X.IOException) ->
      if GE.ioe_type e == GE.NoSuchThing
      then return $ Left "[missing]" -- tool executable not found
      else do putStrLn $ "Warning: IO error attempting to determine " <> forTool <> " version:"
              putStrLn $ show e
              return $ Left "unknown"
  , X.Handler $ \(e :: X.SomeException) -> do
      putStrLn $ "Warning: error attempting to determine " <> forTool <> " version:"
      putStrLn $ show e
      return $ Left "??"
  ]

-- Test Running ----------------------------------------------------------------

-- | Run all provided tests.
main :: IO ()
main =  do
  -- This is a bit more involved than a typical tasty `main` function. The
  -- problem is that the number of tests that we generate (via
  -- `withSugarGroups`) depends on the version of the --llvm-as argument,
  -- which must be checked in IO. Unfortunately, a typical
  -- `defaultMainWithIngredients` invocation doesn't allow you to
  -- generate a dynamic number of tests in IO based on argument values. As a
  -- result, we have to resort to using more of tasty's internals here.
  disasmOpts <- parseCmdLine

  let llvmAs  = TO.lookupOption disasmOpts
      llvmDis = TO.lookupOption disasmOpts

  llvmAsVC <- getLLVMAsVersion llvmAs
  llvmDisVC <- getLLVMDisVersion llvmDis
  unless (vcVersioning llvmAsVC == vcVersioning llvmDisVC) $
    error $ unlines
      [ "Unexpected version mismatch between llvm-as and llvm-dis"
      , "* llvm-as  version: " ++ showVC llvmAsVC
      , "* llvm-dis version: " ++ showVC llvmDisVC
      ]

  sweets <- TS.findSugar cube
  tests <- TS.withSugarGroups sweets testGroup $ \s _ e -> runTest llvmAsVC s e
  case TR.tryIngredients
         disasmTestIngredients
         disasmOpts
         (testGroup "Disassembly tests" [testGroup (showVC llvmAsVC) tests]) of
    Nothing ->
      hPutStrLn IO.stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

  defaultMainWithIngredients disasmTestIngredients $
    testGroup "Disassembly tests" tests

cube :: TS.CUBE
cube = TS.mkCUBE
  { TS.inputDirs = ["disasm-test/tests"]
  , TS.rootName = "*.ll"
  , TS.separators = "."
  , TS.validParams = [ ("llvm-range", Just [ "recent-llvm"
                                           , "pre-llvm12"
                                           , "pre-llvm13"
                                           , "pre-llvm14"
                                           ])
                     ]
    -- Somewhat unusually for tasty-sugar, we make the expectedSuffix the same
    -- as the rootName suffix. This is because we are comparing the contents of
    -- each .ll file against *itself* after parsing it with
    -- llvm-pretty-bc-parser, pretty-printing it with llvm-pretty, and
    -- then normalizing it. As such, each .ll file acts as its own golden file.
  , TS.expectedSuffix = "ll"
  }

-- | A test failure.
data TestFailure
  = ParseError Error -- ^ A parser failure.
    deriving (Typeable,Show)

instance X.Exception TestFailure

-- | Attempt to compare the assembly generated by llvm-pretty and llvm-dis.
runTest :: VersionCheck -> TS.Sweets -> TS.Expectation -> IO [TestTree]
runTest llvmVer sweet expct
  | not llvmMatch
  = pure []
  | otherwise
  = do -- If an .ll file begins with SKIP_TEST, skip that test entirely. For
       -- test cases that require a minimum LLVM version, this technique is
       -- used to prevent running the test on older LLVM versions.
       skipTest <- ("SKIP_TEST" `L.isPrefixOf`) <$> L.readFile (TS.expectedFile expct)

       if skipTest
         then pure []
         else pure $ (:[]) $
           askOption $ \llvmAs ->
           askOption $ \llvmDis ->
           askOption $ \roundtrip ->
           askOption $ \k@(Keep keep) ->
           testCase pfx $ do

             let -- Assemble and disassemble some LLVM asm
                 processLL :: FilePath -> IO (FilePath, Maybe FilePath)
                 processLL f = do
                   putStrLn (showString f ": ")
                   X.handle logError                               $
                     withFile  (generateBitCode    llvmAs  pfx f)  $ \ bc   ->
                     withFile  (normalizeBitCode k llvmDis pfx bc) $ \ norm -> do
                       (parsed, ast) <- processBitCode k roundtrip pfx bc
                       ignore (Proc.callProcess "diff" ["-u", norm, parsed])
                       putStrLn ("successfully parsed " ++ show f)
                       return (parsed, ast)

                 withFile :: IO FilePath -> (FilePath -> IO r) -> IO r
                 withFile iofile f =
                   X.bracket iofile (if keep then const (pure ()) else removeFile) f

             (parsed1, ast) <- processLL file
             case ast of               -- this Maybe also encodes the data of optRoundtrip
               Nothing   -> return ()
               Just ast1 -> do
                 (_, Just ast2) <- processLL parsed1 -- Re-assemble and re-disassemble
                 diff ast1 ast2                      -- Ensure that the ASTs match
                 -- Ensure that the disassembled files match.
                 -- This is usually too strict (and doesn't really provide more info).
                 -- We normalize the AST (see below) to ensure that the ASTs match modulo
                 -- metadata numbering, but the equivalent isn't possible for the
                 -- assembly: we need llvm-as to be able to re-assemble it.
                 -- diff parsed1 parsed2
  where file  = TS.rootFile sweet
        pfx   = TS.rootBaseName sweet
        assertF ls = assertFailure $ unlines ls
        logError (ParseError msg) =
          assertFailure $ unlines $
            "failure" : map ("; " ++) (lines (formatError msg))
        diff file1 file2 = do
          (code, stdout, stderr) <-
            Proc.readCreateProcessWithExitCode (Proc.proc "diff" ["-u", file1, file2]) ""
          case code of
            ExitFailure _ -> assertF ["diff failed", stdout, stderr]
            ExitSuccess   ->
              if stdout /= "" || stderr /= ""
              then assertF ["non-empty diff", stdout, stderr]
              else mapM_ putStrLn ["success: empty diff: ", file1, file2]

        -- Match any LLVM version range specification in the .ll
        -- expected file against the current version of the LLVM tools.
        -- This implements a combination of Case 3 and Case 3a from this
        -- tasty-sugar document:
        -- https://github.com/kquick/tasty-sugar/blob/1fc06bee124e02f49f6478bc1e1df13704cc4916/Ranges.org#case-3---explicit-and-a-weaker-match
        -- In particular, we use `recent-llvm` as an explicit super-supremum
        -- (as in Case 3a), but we also consult the set of Expectations in the
        -- full Sweets value to avoid generating duplicate tests for
        -- `recent-llvm` (as described in Case 3).
        llvmMatch =
          let allMatchingExpectations =
                filter
                  (\e -> (pfx ++ ".") `isPrefixOf` takeFileName (TS.expectedFile e))
                  (TS.expected sweet)

              supportedPreLLVMs :: Set Word
              supportedPreLLVMs =
                Set.fromList $
                mapMaybe
                  (\e -> do
                    TS.Explicit v <- lookup "llvm-range" (TS.expParamsMatch e)
                    verStr <- stripPrefix "pre-llvm" v
                    ver <- readMaybe verStr
                    guard $ vcLT llvmVer ver
                    pure ver)
                  allMatchingExpectations

              -- Implement the "check" step described in Case 3/3a of the
              -- tasty-sugar document linked above.
              specMatchesInstalled v =
                or [ case stripPrefix "pre-llvm" v of
                       Nothing -> False
                       Just verStr
                         |  Just ver <- readMaybe verStr
                         -- Check that the current LLVM version is less than
                         -- the <ver> in the `pre-llvm<ver>` file...
                         ,  vcLT llvmVer ver
                         -- ...moreover, also check that <ver> is the closest
                         -- `pre-llvm` version (without going over). For
                         -- instance, if the current LLVM version is 10 and
                         -- there are both `pre-llvm11` and `pre-llvm12`
                         -- `.ll` files, we only want to run with the
                         -- `pre-llvm11` configuration to avoid duplicate
                         -- tests.
                         ,  Just closestPreLLVM <- Set.lookupMin supportedPreLLVMs
                         -> ver == closestPreLLVM
                         |  otherwise
                         -> False
                     -- as a fallback, if the testing code here is
                     -- unable to determine the version, run all
                     -- tests. This is likely to cause a failure, but
                     -- is preferable to running no tests, which
                     -- results in a success report without having
                     -- done anything.
                   , vcMajor llvmVer == "[missing]"
                   ]
          in -- Implement the "filter" step described in Case 3/3a of the
             -- tasty-sugar document linked above.
             case lookup "llvm-range" (TS.expParamsMatch expct) of
               Just (TS.Explicit v)
                 -- Explicit matches are always allowed.
                 -> specMatchesInstalled v
               Just (TS.Assumed  v)
                 -- The only allowable Assumed match is for `recent-llvm`, the
                 -- super-supremum value...
                 |  v == "recent-llvm"
                 -> case Set.lookupMax supportedPreLLVMs of
                      -- ...if there are no `pre-llvm` .ll files, then allow
                      -- it...
                      Nothing -> True
                      -- ...otherwise, check that the current LLVM version is
                      -- larger than anything specified by a `pre-llvm` .ll
                      -- file.
                      Just largestPreLLVM -> vcGE llvmVer largestPreLLVM
                 |  otherwise
                 -> False
               _ -> error "llvm-range unknown"

-- | Assemble some llvm assembly, producing a bitcode file in /tmp.
generateBitCode :: LLVMAs -> FilePath -> FilePath -> IO FilePath
generateBitCode (LLVMAs llvmAs) pfx file = do
  tmp    <- getTemporaryDirectory
  (bc,h) <- openBinaryTempFile tmp (pfx <.> "bc")
  hClose h
  callProc llvmAs ["-o", bc, file]
  return bc

-- | Use llvm-dis to parse a bitcode file, to obtain a normalized version of the
-- llvm assembly.
normalizeBitCode :: Keep -> LLVMDis -> FilePath -> FilePath -> IO FilePath
normalizeBitCode _keep (LLVMDis llvmDis) pfx file = do
  tmp      <- getTemporaryDirectory
  (norm,h) <- openTempFile tmp (pfx ++ "llvm-dis" <.> "ll")
  hClose h
  callProc llvmDis ["-o", norm, file]
  -- stripComments _keep norm
  return norm

-- | Usually, the ASTs aren't "on the nose" identical.
-- The big thing is that the metadata numbering differs, so we zero out all
-- metadata indices and sort the unnamed metadata list.
-- Done with SYB (Scrap Your Boilerplate).
normalizeModule :: AST.Module -> AST.Module
normalizeModule = sorted . everywhere (mkT zeroValMdRef)
                         . everywhere (mkT zeroNamedMd)
  where sorted m = m { AST.modUnnamedMd =
                         sort (map (\um -> um { AST.umIndex = 0 })
                                   (AST.modUnnamedMd m)) }
        -- Zero out all ValMdRefs
        zeroValMdRef (AST.ValMdRef _) = AST.ValMdRef 0
        zeroValMdRef a                = (a :: AST.ValMd) -- avoid ambiguous type

        -- Reduce all named metadata
        zeroNamedMd (AST.NamedMd x _) = AST.NamedMd x []


-- | Parse a bitcode file using llvm-pretty, failing the test if the parser
-- fails.
processBitCode :: Keep -> Roundtrip -> FilePath -> FilePath -> IO (FilePath, Maybe FilePath)
processBitCode _keep (Roundtrip roundtrip) pfx file = do
  let handler :: X.SomeException -> IO (Either Error AST.Module)
      handler se = return (Left (Error [] (show se)))
      printToTempFile sufx stuff = do
        tmp        <- getTemporaryDirectory
        (parsed,h) <- openTempFile tmp (pfx ++ "llvm-disasm" <.> sufx)
        hPutStrLn h stuff
        hClose h
        return parsed
  e <- parseBitCodeLazyFromFile file `X.catch` handler
  case e of
    Left err -> X.throwIO (ParseError err)
    Right m  -> do
      parsed <- printToTempFile "ll" (show (ppLLVM (ppModule m)))
      -- stripComments _keep parsed
      if roundtrip
      then do
        tmp2 <- printToTempFile "ast" (ppShow (normalizeModule m))
        return (parsed, Just tmp2)
      else return (parsed, Nothing)

-- | Remove comments from a .ll file, stripping everything including the
-- semi-colon.
stripComments :: Keep -> FilePath -> IO ()
stripComments (Keep keep) path = do
  bytes <- L.readFile path
  when (not keep) (removeFile path)
  mapM_ (writeLine . dropComments) (bsLines bytes)
  where
  writeLine bs | L.null bs = return ()
               | otherwise = do
                 L.appendFile path bs
                 L.appendFile path (L.singleton 0x0a)

-- | Split a bytestring by its lines.
bsLines :: L.ByteString -> [L.ByteString]
bsLines = L.split char
  where
  char = fromIntegral (ord '\n')

-- | Take characters until the llvm comment delimiter is found.
dropComments :: L.ByteString -> L.ByteString
dropComments  = dropTrailingSpace . L.takeWhile (/= char)
  where
  char = fromIntegral (ord ';')

-- | Drop trailing space from a bytestring.
dropTrailingSpace :: L.ByteString -> L.ByteString
dropTrailingSpace bs
  | len <= 0  = L.empty
  | otherwise = L.take (loop len) bs
  where
  len = L.length bs - 1
  loop n | isSpace (chr (fromIntegral (L.index bs n))) = loop (n-1)
         | otherwise                                   = n

-- | Ignore a command that fails.
ignore :: IO () -> IO ()
ignore  = X.handle f
  where f   :: X.IOException -> IO ()
        f _ = return ()

callProc :: String -> [String] -> IO ()
callProc p args = -- putStrLn ("Calling process: " ++ p ++ " " ++ unwords args) >>
  Proc.callProcess p args
