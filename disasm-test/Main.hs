{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.LLVM.BitCode (parseBitCodeLazyFromFileWithWarnings,
                                    Error(..),formatError,
                                    ParseWarning,ppParseWarnings)
import qualified Text.LLVM.AST as AST
import           Text.LLVM.PP ( ppLLVM, ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38, llvmPP )

import qualified Control.Exception as EX
import           Control.Lens ( (^?), _Right )
import           Control.Monad ( foldM, unless, when )
import qualified Control.Monad.Catch as X
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans.State
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as L
import           Data.Char (ord,isLetter,isSpace,chr)
import           Data.Generics (everywhere, mkT) -- SYB
import           Data.List ( find, isInfixOf, isPrefixOf, isSuffixOf, nub, sort )
import           Data.Map ( (!), (!?) )
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )
import           Data.Proxy ( Proxy(..) )
import           Data.Sequence ( Seq )
import           Data.String.Interpolate
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Versions (Versioning, versioning, prettyV, major, minor)
import qualified GHC.IO.Exception as GE
import qualified Options.Applicative as OA
import qualified Prettyprinter as PP
import qualified Prettyprinter.Util as PPU
import qualified System.Console.Terminal.Size as Term
import           System.Directory ( doesFileExist, getTemporaryDirectory
                                  , listDirectory
                                  , removeFile )
import           System.Environment ( lookupEnv )
import           System.Exit (ExitCode(..), exitFailure, exitSuccess)
import           System.FilePath ( (</>), (<.>) )
import           System.IO (openBinaryTempFile,hClose,openTempFile,hPrint,
                            hPutStrLn,stderr)
import qualified System.IO as IO (stderr)
import qualified System.Process as Proc
import           Test.Tasty
import           Test.Tasty.ExpectedFailure ( ignoreTestBecause
                                            , expectFailBecause )
import           Test.Tasty.HUnit ( assertFailure, testCase )
import qualified Test.Tasty.Options as TO
import qualified Test.Tasty.Runners as TR
import qualified Test.Tasty.Sugar as TS
import           Text.Read (readMaybe)
import           Text.Show.Pretty (ppShow)


descr :: PP.Doc ann
descr = PP.vcat $
  let block = PPU.reflow in
  [ block [iii|
 This test verifies that the llvm-pretty-bc-parser is capable of
 parsing bitcode properly.  This is done by two sets of operations:
 one which uses the llvm-as assembler to generate bitcode from .ll files
 (text assembly in LLVM format), and one which uses the clang compiler to
 generate bitcode from C files; this library should be able to parse both
 types of generated bitcode files.
      |]
  , ""
  , block [iii|
 The assembler test method starts with a known .ll file (LLVM text assembly
 format) and assembles it to LLVM bitcode (via llvm-as from the LLVM tool
 suite). Then the test will use both llvm-dis (from the LLVM tool suite)
 and llvm-disasm (from this package, via direct library calls) to convert
 that bitcode back into the .ll text format, and also (for the latter) into
 an AST representation.  [NOTE: C sources should be kept minimal to be focused
 in particular; if too large there are too many opportunities for unrelated
 issues to interfere in successful evaluation.]
       |]
  , ""
  , block [iii|
 The compiler test method starts with a known .c or .cpp file and uses clang to directly
 generate a bitcode file.  The test then proceeds just as with the assembler
 test.  The only difference therefore is the starting file and first command
 used on that file, but the compiler method will usually generate more variance
 in the bitcode files as the compiler version changes.
       |]
  , ""
  , "          +-------------[2nd cycle]------------+"
  , "          |                                    |"
  , "          v                                    |"
  , " .ll --[llvm-as]--> .bc ---[llvm-dis]--> .ll   |"
  , "                     ^   `-[llvm-disasm]---> .ll"
  , "                     |                   `-> .AST"
  , " .c/.cpp --[clang]---+                         |"
  , "                     |                        [show]"
  , " .bc -[pre-existing]-+                         |"
  , "                                               v"
  , "      [compare first and second of these:]   .txt"
  , ""
  , block [iii|
 The differences between the two .ll text formats
 are displayed for user information, but differences do not constitute a
 test failure.  The .ll text file obtained from llvm-disasm is then
 *re-assembled* into another bitcode file, which is again converted back to
 both .ll and AST formats (repeating the above actions, thus any
 differences in the .ll files may be displayed twice.  Finally, the two
 AST's are serialized to text and the resulting text is compared for
 differences.  The tests will fail if any of the conversion steps fail, or
 if there are differences between the first round-trip .ll file and the
 second round-trip .ll file produced by llvm-pretty-bc-parser.
       |]
  , ""
  , PP.indent 2 $ PP.hang 2 $ block
    [iii|* If opaque pointers are present, *all* pointers are
           converted to opaque pointers.  This is because the LLVM
           tools are stricter about opaque pointers, whereas this
           package is more permissive. Opaque pointers are
           the standard in LLVM 15.
      |]
  , ""
  , PP.indent 2 $ PP.hang 2 $ block
    [iii|* Metadata information between the two AST text formats is *not*
           compared (and is actually discarded before serialization, resetting
           all indices to zero).  This is because metadata indexes are
           dynamically generated and therefore unstable.
        |]
  , ""
  , PP.indent 2 $ PP.hang 2 $ block
    [iii|* If the --roundtrip-disabled option is specified on the command line,
           then only one assembly-disassembly will be performed and
           the results will not be compared to anything (although any
           llvm-dis/llvm-disasm differences will still be shown).
           This mode ensures that the llvm-disasm will not fail, but
           it does not validate the results.
        |]
  , ""
  , block
    [iii|The DISASM_TEST_DIR environment variable can be set to the
         location of the disasm-test directory; this defaults to
         "disasm-test", but that is not useful unless the test is
         run from the top level of the llvm-pretty-bc-parser
         repository.
        |]
  ]

-- Option Parsing --------------------------------------------------------------

newtype LLVMAs = LLVMAs FilePath

defaultLLVMAs :: LLVMAs
defaultLLVMAs = LLVMAs "llvm-as"

instance TO.IsOption LLVMAs where
  defaultValue = defaultLLVMAs
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

newtype Clang = Clang FilePath

instance TO.IsOption Clang where
  defaultValue = Clang "clang"
  parseValue = Just . Clang
  optionName = pure "with-clang"
  optionHelp = pure "path to clang"
  showDefaultValue (Clang dis) = Just dis
  optionCLParser = TO.mkOptionCLParser $
    OA.metavar "FILEPATH"

newtype Roundtrip = Roundtrip Bool

instance TO.IsOption Roundtrip where
  defaultValue = Roundtrip True
  parseValue = fmap Roundtrip . TO.safeReadBool
  optionName = pure "roundtrip-disabled"
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

newtype Details = Details Bool

instance TO.IsOption Details where
  defaultValue = Details False
  parseValue = fmap Details . TO.safeReadBool
  optionName = pure "details"
  optionHelp = pure "show details of each individual test execution (for debug)"
  showDefaultValue (Details d) = Just $ show d
  optionCLParser = TO.mkOptionCLParser $
    OA.short 'd'

disasmTestIngredients :: FilePath -> VersionCheck -> [TR.Ingredient]
disasmTestIngredients rootPath llvmver =
  includingOptions [ TO.Option (Proxy @LLVMAs)
                   , TO.Option (Proxy @LLVMDis)
                   , TO.Option (Proxy @Clang)
                   , TO.Option (Proxy @Roundtrip)
                   , TO.Option (Proxy @Keep)
                   , TO.Option (Proxy @Details)
                   ] :
  TS.sugarIngredients [ assemblyCube rootPath llvmver
                      , cCompilerCube rootPath llvmver
                      , ccCompilerCube rootPath llvmver ]
  <> defaultIngredients

parseCmdLine :: IO TO.OptionSet
parseCmdLine = do
  TR.installSignalHandlers
  llvmver <- getLLVMAsVersion defaultLLVMAs
  rootPth <- getRootPath
  let disasmOptDescrs = TO.uniqueOptionDescriptions $
        TR.coreOptions ++
        TS.sugarOptions ++
        TR.ingredientsOptions (disasmTestIngredients rootPth llvmver)
      (disasmOptWarns, disasmOptParser) = TR.optionParser disasmOptDescrs
  mapM_ (hPutStrLn IO.stderr) disasmOptWarns
  ts <- maybe 80 Term.width <$> Term.size
  let pr = OA.prefs $ OA.columns ts
  OA.customExecParser pr $
    OA.info (OA.helper <*> disasmOptParser)
    ( OA.fullDesc
      <> OA.header "llvm-pretty-bc-parser disassembly test suite"
      <> OA.footerDoc (Just $ PP.align descr)
    )


-- Querying Tool Versions ------------------------------------------------------

-- | Captures the name of the tool and either the error when attempting to get
-- the tool version or the actual parsed version self-reported by the tool.  Lack
-- of a decipherable version is not fatal to running the tests.
data VersionCheck = VC String (Either T.Text Versioning)

showVC :: VersionCheck -> String
showVC (VC nm v) = nm <> " " <> (T.unpack $ either id prettyV v)

vcVersioning :: VersionCheck -> Either T.Text Versioning
vcVersioning (VC _ v) = v

mkVC :: String -> String -> VersionCheck
mkVC nm raw = let r = T.pack raw in VC nm $ first (const r) $ versioning r

getLLVMAsVersion :: LLVMAs -> IO VersionCheck
getLLVMAsVersion (LLVMAs llvmAsPath) = getLLVMToolVersion "llvm-as" llvmAsPath

getLLVMDisVersion :: LLVMDis -> IO VersionCheck
getLLVMDisVersion (LLVMDis llvmDisPath) = getLLVMToolVersion "llvm-dis" llvmDisPath

getClangVersion :: Clang -> IO VersionCheck
getClangVersion (Clang clangPath) = getLLVMToolVersion "clang" clangPath

-- Determine which version of an LLVM tool will be used for these tests (if
-- possible).  Uses partial 'head' but this is just tests, and failure is
-- captured.
getLLVMToolVersion :: String -> FilePath -> IO VersionCheck
getLLVMToolVersion toolName toolPath = do
  let isVerLine l = isInfixOf "LLVM version" l || isInfixOf "clang version" l
      dropLetter = dropWhile (all isLetter)
      getVer (Right inp) =
        -- example inp: "LLVM version 10.0.1" or "clang version 11.1.0"
        case filter isVerLine $ lines inp of
          [] -> "NO VERSION IDENTIFIED FOR " <> toolName
          (l:_) -> case dropLetter $ words l of
            [] -> toolName <> " VERSION NOT PARSED: " <> l
            (v:_) -> fst $ break (== '-') v -- remove vendor suffix (e.g. 12.0.1-19ubuntu3)
      getVer (Left full) = full
  mkVC toolName . getVer <$> readProcessVersion toolPath

-- Runs the tool with a --version argument to have it self-report its version.
-- The tool may not even be installed.  Returns either an error string or the
-- output string from the tool.
readProcessVersion :: String -> IO (Either String String)
readProcessVersion forTool =
  X.catches (Right <$> Proc.readProcess forTool [ "--version" ] "")
  [ X.Handler $ \(e :: EX.IOException) ->
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
  rootPth <- getRootPath
  -- This is a bit more involved than a typical tasty `main` function. The
  -- problem is that the number of tests that we generate (via
  -- `withSugarGroups`) depends on the version of the --llvm-as argument,
  -- which must be checked in IO. Unfortunately, a typical
  -- `defaultMainWithIngredients` invocation doesn't allow you to
  -- generate a dynamic number of tests in IO based on argument values. As a
  -- result, we have to resort to using more of tasty's internals here.
  disasmOpts <- parseCmdLine

  let llvmAs'  = TO.lookupOption disasmOpts
      llvmDis' = TO.lookupOption disasmOpts
      clang'   = TO.lookupOption disasmOpts

  llvmAsVC <- getLLVMAsVersion llvmAs'
  llvmDisVC <- getLLVMDisVersion llvmDis'
  clangVC <- getClangVersion clang'
  unless (and [ vcVersioning llvmAsVC == vcVersioning llvmDisVC
              , vcVersioning llvmAsVC == vcVersioning clangVC
              ]) $
    error $ unlines
      [ "Unexpected version mismatch between clang, llvm-as and llvm-dis"
      , "* llvm-as  version: " ++ showVC llvmAsVC
      , "* llvm-dis version: " ++ showVC llvmDisVC
      , "* clang    version: " ++ showVC clangVC
      ]

  knownBugs <- getKnownBugs rootPth
  sweets1 <- TS.findSugar $ assemblyCube rootPth llvmAsVC
  sweets2 <- TS.findSugar $ cCompilerCube rootPth llvmAsVC
  sweets3 <- TS.findSugar $ ccCompilerCube rootPth llvmAsVC
  sweets4 <- TS.findSugar $ bitcodeCube rootPth llvmAsVC
  atests <- TS.withSugarGroups sweets1 testGroup
            $ \s _ e -> runAssemblyTest llvmAsVC knownBugs s e
  ctests <- TS.withSugarGroups sweets2 testGroup
            $ \s _ e -> runCompileTest llvmAsVC knownBugs s e
  cctests <- TS.withSugarGroups sweets3 testGroup
             $ \s _ e -> runCompileTest llvmAsVC knownBugs s e
  bctests <- TS.withSugarGroups sweets4 testGroup
             $ \s _ e -> runRawBCTest llvmAsVC knownBugs s e
  let tests = atests <> ctests
  case TR.tryIngredients
         (disasmTestIngredients rootPth llvmAsVC)
         disasmOpts
         (testGroup "Disassembly tests"
          [ testGroup ("llvm-as " <> showVC llvmAsVC) atests
          , testGroup ("C " <> showVC clangVC) ctests
          , testGroup ("C++ " <> showVC clangVC) cctests
          , testGroup ("rawBC " <> showVC llvmAsVC) bctests
          ]) of
    Nothing ->
      hPutStrLn IO.stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

  defaultMainWithIngredients (disasmTestIngredients rootPth llvmAsVC) $
    testGroup "Disassembly tests" tests

----------------------------------------------------------------------
-- Assembly/disassembly tests

getRootPath :: IO FilePath
getRootPath = maybe "disasm-test" id <$> lookupEnv "DISASM_TEST_DIR"

assemblyCube :: FilePath -> VersionCheck -> TS.CUBE
assemblyCube rootPath llvmver = TS.mkCUBE
  { TS.inputDirs = [rootPath </> "tests"]
  , TS.rootName = "*.ll"
  , TS.separators = "."
  , TS.validParams = [ ("llvm-range", Just [ "recent-llvm"
                                           , "pre-llvm9"
                                           , "pre-llvm12"
                                           , "pre-llvm13"
                                           , "pre-llvm14"
                                           , "pre-llvm15"
                                           , "pre-llvm16"
                                           , "pre-llvm17"
                                           , "pre-llvm18"
                                           , "post-llvm18"
                                           , "pre-llvm19"
                                           , "pre-llvm20"
                                           ])
                     ]
    -- Somewhat unusually for tasty-sugar, we make the expectedSuffix the same
    -- as the rootName suffix. This is because we are comparing the contents of
    -- each .ll file against *itself* after parsing it with
    -- llvm-pretty-bc-parser, pretty-printing it with llvm-pretty, and
    -- then normalizing it. As such, each .ll file acts as its own golden file.
  , TS.expectedSuffix = "ll"
  , TS.sweetAdjuster = \cb ->
      -- In addition to range matching, this is a round-trip test (assemble +
      -- disassemble) where the rootname is the same as the expected name.
      -- Filter out any expectations that don't match the root name.
      -- (e.g. remove: root=poison.ll with exp=poison.pre-llvm12.ll).
      let rootExpSame s e = TS.rootFile s == TS.expectedFile e
          addExpFilter s = s { TS.expected = filter (rootExpSame s) $ TS.expected s }
      in fmap (fmap addExpFilter) . rangeMatch llvmver cb
  }


rangeMatch :: MonadIO m => VersionCheck -> TS.CUBE -> [TS.Sweets] -> m [TS.Sweets]
rangeMatch llvmver cb swts = do
  -- Perform ranged-matching of the llvm-range parameter against the version of
  -- llvm (reported by llvm-as) to filter the tasty-sugar expectations.  Note
  -- that there is a built-in expectation here that there is only one llvm
  -- version available to the test.
  ts1 <- TS.rangedParamAdjuster "llvm-range"
    (readMaybe . drop (length ("pre-llvm" :: String)))
    (<)
    (vcVersioning llvmver ^? (_Right . major))
    cb swts
  TS.rangedParamAdjuster "llvm-range"
    (readMaybe . drop (length ("post-llvm" :: String)))
    (>)
    (vcVersioning llvmver ^? (_Right . major))
    cb ts1


-- | Returns true if this particular test should be skipped, which is signalled
-- by the expected file contents starting with "SKIP_TEST".  For test cases that
-- require a minimum LLVM version, this technique is used to prevent running the
-- test on older LLVM versions.
skipTest :: TS.Expectation -> IO Bool
skipTest expct =
  ("SKIP_TEST" `L.isPrefixOf`) <$> L.readFile (TS.expectedFile expct)


-- | Attempt to compare the assembly generated by llvm-pretty and llvm-dis.
runAssemblyTest :: VersionCheck -> KnownBugs -> TS.Sweets -> TS.Expectation
                -> IO [TestTree]
runAssemblyTest llvmVersion knownBugs sweet expct
  = do shouldSkip <- skipTest expct
       let tmod = if shouldSkip
                  then ignoreTestBecause "not valid for this LLVM version"
                  else case isKnownBug knownBugs sweet expct llvmVersion of
                         Just (from, why) ->
                           expectFailBecause $ why <> " [see " <> from <> "]"
                         Nothing -> id
       let pfx = TS.rootBaseName sweet
       return $ (:[]) $ tmod
         $ testCaseM llvmVersion pfx
         $ with2Files (processLL pfx $ TS.rootFile sweet)
         $ \(parsed1, ast) ->
             case ast of
               Nothing   -> return ()
               Just ast1 ->
                 -- Re-assemble and re-disassemble
                 with2Files (processLL pfx parsed1)
                 $ \(_, mb'ast2) ->
                     case mb'ast2 of
                       Just ast2 -> diffCmp ast1 ast2 -- Ensure that the ASTs match

                                    -- Ensure that the disassembled files match.
                                    -- This is usually too strict (and doesn't
                                    -- really provide more info).  We normalize
                                    -- the AST (see below) to ensure that the
                                    -- ASTs match modulo metadata numbering, but
                                    -- the equivalent isn't possible for the
                                    -- assembly: we need llvm-as to be able to
                                    -- re-assemble it.
                                    --
                                    -- diffCmp parsed1 parsed2
                       Nothing -> error "Failed processLL"


diffCmp :: FilePath -> FilePath -> TestM ()
diffCmp file1 file2 = do
  let assertF = liftIO . assertFailure . unlines
  (code, stdOut, stdErr) <- liftIO $
    Proc.readCreateProcessWithExitCode (Proc.proc "diff" ["-u", file1, file2]) ""
  case code of
    ExitFailure _ -> assertF ["diff failed", stdOut, stdErr]
    ExitSuccess   ->
      if stdOut /= "" || stdErr /= ""
      then assertF ["non-empty diff", stdOut, stdErr]
      else do Details det <- gets showDetails
              when det $ liftIO
                $ mapM_ putStrLn ["success: empty diff: ", file1, file2]


-- Assembles the specified .ll file to bitcode, then disassembles it with
-- llvm-dis.  Also parses the bitcode with this library (effectively llvm-disasm)
-- and prints the difference between the parsed version and the .ll file.
-- Returns the library parsed version and the serialized AST from the library.

processLL :: FilePath -> FilePath -> TestM (FilePath, Maybe FilePath)
processLL pfx f = do
  Details det <- gets showDetails
  when det $ liftIO $ putStrLn (showString f ": ")
  X.handle logError
    $ withFile (assembleToBitCode pfx f)
    $ parseBC pfx
  where
    logError (ParseError msg) =
      liftIO $ assertFailure $ unlines
      $ "failure" : map ("; " ++) (lines (formatError msg))

parseBC :: FilePath -> FilePath -> TestM (FilePath, Maybe FilePath)
parseBC pfx bc = do
  withFile (X.handle
            (\(_ :: GE.IOException) -> return "LLVM llvm-dis failed to parse this file")
            (disasmBitCode pfx bc))
    $ \ norm -> do
    (parsed, ast) <- processBitCode pfx bc
    Details dets <- gets showDetails
    when dets $ liftIO $ do
      -- Informationally display if there are differences between the llvm-dis
      -- and llvm-disasm outputs, but no error if they differ.  Note that the
      -- arguments to this diff are not the same as those supplied to the diffCmp
      -- function. The diff here is intended to supply additional information to
      -- the user for diagnostics, and whitespace changes are likely unimportant
      -- in that context; this diff does not determine the pass/fail status of
      -- the testing.  On the other hand, the diffCmp *does* determine if the
      -- tests pass or fail, so ignoring whitespace in that determination would
      -- potentially weaken the testing to an unsatisfactory degree and would
      -- need more careful evaluation.
      putStrLn "## Output differences: LLVM's llvm-dis <--> this llvm-disasm"
      ignore (Proc.callProcess "diff" ["-u", "-b", "-B", "-w", norm, parsed])
      putStrLn ("successfully parsed " ++ show pfx ++ " bitcode")
    return (parsed, ast)

----------------------------------------------------------------------
-- Compiler->Assembly->Disassembly tests

-- The compilation tests ensure that the clang version-specific generated .bc
-- file can be reasonably parsed by this library.  This is a parallel to the
-- assemblyCube-driven tests, but starts with a C source file.  One distinction
-- is that the .ll used for the assemblyCube is typically representative of a
-- specific LLVM version, and while it is assembled and disassembled by newer
-- versions of LLVM tools, it will never introduce any newer element, whereas the
-- clang-generated bitcode will contain version-current output which might have
-- newer elements and ordering.
--
-- The cCompilerCube uses .c (C source) files as the input and .ll files for the
-- expected output.  The ccCompilerCube uses .cc (C++ source) files as the input
-- and .ll files for the expected output.  The assemblyCube uses the .ll file as
-- both input and output.  The actual testing done is very similar, and the
-- assemblyCube always generates a superset of the compilerCube tests (i.e. when
-- no .c or .cc file is present).

cCompilerCube :: FilePath -> VersionCheck -> TS.CUBE
cCompilerCube rootPath llvmver =
  (assemblyCube rootPath llvmver)
  { TS.rootName = "*.(c|cc|cpp)"
  , TS.sweetAdjuster = rangeMatch llvmver
  }

ccCompilerCube :: FilePath -> VersionCheck -> TS.CUBE
ccCompilerCube rootPath llvmver =
  (cCompilerCube rootPath llvmver) { TS.rootName = "*.cc"}


runCompileTest :: VersionCheck -> KnownBugs -> TS.Sweets -> TS.Expectation
               -> IO [TestTree]
runCompileTest llvmVersion knownBugs sweet expct = do
  shouldSkip <- skipTest expct
  let tmod = if shouldSkip
             then ignoreTestBecause "not valid for this LLVM version"
             else case isKnownBug knownBugs sweet expct llvmVersion of
                    Just (from, why) ->
                      expectFailBecause $ why <> " [see " <> from <> "]"
                    Nothing -> id
  let pfx = TS.rootBaseName sweet
  return $ (:[]) $ tmod
    $ testCaseM llvmVersion pfx
    $ withFile (compileToBitCode pfx $ TS.rootFile sweet)
    $ \bc ->
        with2Files (parseBC pfx bc)
        $ \(parsed1, ast) ->
            case ast of
              Nothing ->
                -- No round trip, so this just verifies that the bitcode could be
                -- parsed without generating an error.
                return ()
              Just ast1 ->
                -- Assemble and re-parse the bitcode to make sure it can be
                -- round-tripped successfully.
                with2Files (processLL pfx parsed1)
                $ \(_, mb'ast2) -> case mb'ast2 of
                                     Just ast2 -> diffCmp ast1 ast2
                                     Nothing -> error "failed processLL"
                  -- fst is ignored because .ll files are not compared; see
                  -- runAssemblyTest for details.


----------------------------------------------------------------------
-- Pre-existing bitcode tests tests

bitcodeCube :: FilePath -> VersionCheck -> TS.CUBE
bitcodeCube rootPath llvmver =
  (assemblyCube rootPath llvmver)
  { TS.rootName = "*.bc"
  , TS.inputDirs = [rootPath </> "bc_src_tests"]
  , TS.sweetAdjuster = rangeMatch llvmver
  }

runRawBCTest :: VersionCheck -> KnownBugs -> TS.Sweets -> TS.Expectation
               -> IO [TestTree]
runRawBCTest llvmVersion knownBugs sweet expct = do
  shouldSkip <- skipTest expct
  let tmod = if shouldSkip
             then ignoreTestBecause "not valid for this LLVM version"
             else case isKnownBug knownBugs sweet expct llvmVersion of
                    Just (from, why) ->
                      expectFailBecause $ why <> " [see " <> from <> "]"
                    Nothing -> id
  let pfx = TS.rootBaseName sweet
  let bc = TS.rootFile sweet
  return $ (:[]) $ tmod
    $ testCaseM llvmVersion pfx
    $ with2Files (parseBC pfx bc)
        $ \(parsed1, ast) ->
            case ast of
              Nothing ->
                -- No round trip, so this just verifies that the bitcode could be
                -- parsed without generating an error.
                return ()
              Just ast1 ->
                -- Assemble and re-parse the bitcode to make sure it can be
                -- round-tripped successfully.
                with2Files (processLL pfx parsed1)
                $ \(_, mb'ast2) -> case mb'ast2 of
                                     Just ast2 -> diffCmp ast1 ast2
                                     Nothing -> error "Failed processLL"
                  -- fst is ignored because .ll files are not compared; see
                  -- runAssemblyTest for details.


----------------------------------------------------------------------
-- Helpers

-- | A test failure.
data TestFailure
  = ParseError Error -- ^ A parser failure
    deriving (Typeable,Show)

instance X.Exception TestFailure


-- This structure essentially recapitulates the TestOptions, but in a way that
-- they will be accessible in a TestTree (via: StateT TestState IO a).
data TestState = TestState { keepTemp :: Keep
                           , rndTrip :: Roundtrip
                           , showDetails :: Details
                           , llvmAs :: LLVMAs
                           , llvmDis :: LLVMDis
                           , clang :: Clang
                           , llvmVer :: VersionCheck
                           }

type TestM a = StateT TestState IO a

testCaseM :: VersionCheck -> FilePath -> TestM () -> TestTree
testCaseM llvmVersion pfx ops =
  askOption $ \llvmAs' ->
  askOption $ \llvmDis' ->
  askOption $ \roundtrip ->
  askOption $ \keep ->
  askOption $ \details ->
  askOption $ \clang' ->
  testCase pfx $ evalStateT ops (TestState { keepTemp = keep
                                           , rndTrip = roundtrip
                                           , showDetails = details
                                           , llvmAs = llvmAs'
                                           , llvmDis = llvmDis'
                                           , clang = clang'
                                           , llvmVer = llvmVersion
                                           })


-- | Assemble some llvm assembly, producing a bitcode file in /tmp.
assembleToBitCode :: FilePath -> FilePath -> TestM FilePath
assembleToBitCode pfx file = do
  tmp <- liftIO getTemporaryDirectory
  LLVMAs asm <- gets llvmAs
  X.bracketOnError
    (liftIO $ openBinaryTempFile tmp (pfx <.> "bc"))
    (rmFile . fst)
    $ \(bc,h) ->
        do liftIO $ hClose h
           callProc asm ["-o", bc, file]
           return bc

-- | Compile a C or C++ source, producing a bitcode file in /tmp.
compileToBitCode :: FilePath -> FilePath -> TestM FilePath
compileToBitCode pfx file = do
  tmp <- liftIO getTemporaryDirectory
  Clang comp' <- gets clang
  let comp = if ".cc" `isSuffixOf` file then comp' <> "++" else comp'
  X.bracketOnError
    (liftIO $ openBinaryTempFile tmp (pfx <.> "bc"))
    (rmFile . fst)
    $ \(bc,h) ->
        do liftIO $ hClose h
           callProc comp ["-c", "-emit-llvm", "-O0", "-U_FORTIFY_SOURCE", "-g", "-o", bc, file]
           return bc

-- | Use llvm-dis to parse a bitcode file, to obtain a normalized version of the
-- llvm assembly.
disasmBitCode :: FilePath -> FilePath -> TestM FilePath
disasmBitCode pfx file = do
  tmp <- liftIO $ getTemporaryDirectory
  LLVMDis dis <- gets llvmDis
  X.bracketOnError
    (liftIO $ openTempFile tmp (pfx ++ "llvm-dis" <.> "ll"))
    (rmFile . fst)
    $ \(norm,h) ->
        do liftIO $ hClose h
           callProc dis ["-o", norm, file]
           -- stripComments norm
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
processBitCode :: FilePath -> FilePath -> TestM (FilePath, Maybe FilePath)
processBitCode pfx file = do
  let handler ::
        X.SomeException -> IO (Either Error (AST.Module, Seq ParseWarning))
      handler se = return (Left (Error [] (show se)))
      printToTempFile sufx stuff = do
        tmp        <- getTemporaryDirectory
        (parsed,h) <- openTempFile tmp (pfx ++ "llvm-disasm" <.> sufx)
        hPutStrLn h stuff
        hClose h
        return parsed
  e <- liftIO $ parseBitCodeLazyFromFileWithWarnings file `X.catch` handler
  case e of
    Left err -> X.throwM (ParseError err)
    Right (m, warnings) -> do
      unless (null warnings) $
        liftIO $ hPrint stderr $ ppParseWarnings warnings
      let m' = AST.fixupOpaquePtrs m
      postParseTests m'
      llvmVersion <- gets llvmVer
      llvmAssembly <-
        case vcVersioning llvmVersion ^? (_Right . major) of
          Nothing -> do liftIO $ hPutStrLn IO.stderr
                          ( "warning: unknown LLVM version ("
                            <> showVC llvmVersion <> "), assuming 3.5")
                        return $ ppLLVM35 $ llvmPP m'
          Just v ->
            case v of
              3 -> case vcVersioning llvmVersion ^? (_Right . minor) of
                     Just 5 -> return $ ppLLVM35 $ llvmPP m'
                     Just 6 -> return $ ppLLVM36 $ llvmPP m'
                     Just 7 -> return $ ppLLVM37 $ llvmPP m'
                     Just 8 -> return $ ppLLVM38 $ llvmPP m'
                     o -> if maybe True (< 5) o
                          then return $ ppLLVM35 $ llvmPP m'
                          else return $ ppLLVM38 $ llvmPP m'
              _ -> return $ ppLLVM (fromEnum v) $ llvmPP m'
      parsed <- liftIO $ printToTempFile "ll" $ show llvmAssembly
      Roundtrip roundtrip <- gets rndTrip
      -- stripComments parsed
      Details det <- gets showDetails
      if roundtrip
      then do
        tmp2 <- liftIO $ printToTempFile "ast" (ppShow (normalizeModule m'))
        when det $ liftIO $ putStrLn $ "## parsed Bitcode to " <> parsed <> " and " <> tmp2
        return (parsed, Just tmp2)
      else do
        when det $ liftIO $ putStrLn $ "## parsed Bitcode to " <> parsed
        return (parsed, Nothing)


-- | These are common tests that should be run on the AST that is parsed from the
-- bitcode file.  This tests invariants that are not accessible or testable from
-- the serialized formats.
postParseTests :: AST.Module -> TestM ()
postParseTests m = ensureValidMetadataIndices m
  where
    -- This test is to ensure that all unnamed metadata instances have a unique
    -- index value.
    ensureValidMetadataIndices md = do
      let idxs = AST.umIndex <$> AST.modUnnamedMd md
      let uniqIdxs = nub idxs
      let numDups = length idxs - length uniqIdxs
      unless (numDups == 0)
        $ do Details det <- gets showDetails
             when det $ liftIO $ putStrLn
               $ "Unnamed metadata (modUnnamedMd) indices: " <> show idxs
             liftIO $ assertFailure
               $ show numDups
               <> " duplicated Unnamed metadata (modUnnamedMd) indices"



-- | Remove comments from a .ll file, stripping everything including the
-- semi-colon.
stripComments :: FilePath -> TestM ()
stripComments path = do
  Keep keep <- gets keepTemp
  bytes <- liftIO $ L.readFile path
  when (not keep) $ rmFile path
  mapM_ (writeLine . dropComments) (bsLines bytes)
  where
  writeLine bs | L.null bs = return ()
               | otherwise = liftIO $ do
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
  where f   :: EX.IOException -> IO ()
        f _ = return ()

callProc :: String -> [String] -> TestM ()
callProc p args = do
  Details dets <- gets showDetails
  when dets $ liftIO $ putStrLn ("## Running: " ++ p ++ " " ++ unwords args)
  liftIO $ Proc.callProcess p args

withFile :: TestM FilePath -> (FilePath -> TestM r) -> TestM r
withFile iofile f = X.bracket iofile rmFile f

with2Files :: TestM (FilePath, Maybe FilePath)
           -> ((FilePath, Maybe FilePath) -> TestM r)
           -> TestM r
with2Files iofiles f =
  let cleanup (tmp1, mbTmp2) = do
        rmFile tmp1
        traverse rmFile mbTmp2
  in X.bracket iofiles cleanup f

rmFile :: FilePath -> TestM ()
rmFile tmp = do Keep keep <- gets keepTemp
                unless keep
                  $ do do exists <- liftIO $ doesFileExist tmp
                          when exists $ do
                            Details dets <- gets showDetails
                            when dets $ liftIO $ putStrLn $ "## Removing " <> tmp
                            liftIO $ removeFile tmp

----------------------------------------------------------------------

-- Handling Known Bugs

-- | A map from a known bug file to the identifiers and identifier values in that
-- known bug file.
type KnownBugs = Map.Map FilePath (Map.Map String [String])

-- | There is a directory containing files which describe known bugs (one per
-- file).  Those files contain marker lines (prefixed with "##> ") which specify
-- the elements and values that should match a test for this test to be a known
-- bug.  This function reads in that file and creates a map from file to a map of
-- markers and values in that file.  This returned value should be passed to the
-- isKnownBug function, along with the test information to determine if the test
-- is associated with a known bug.
getKnownBugs :: FilePath -> IO KnownBugs
getKnownBugs rootPath = do
  let kbdir = rootPath </> "known_bugs"
  known <- listDirectory kbdir
  let interestingLine = ("##> " `isPrefixOf`)
  let addInterestingLine l = case words l of
                               (_ : t : ws) -> Map.insertWith (<>) t ws
                               _ -> id
  let interestingLineMap ls = foldr addInterestingLine mempty
                              $ filter interestingLine ls
  let addKnownBugInfo mp f = do
        let fpath = kbdir </> f
        X.try (lines <$> readFile fpath) >>= \case
          Left (_e :: IOError) ->
            -- Error reading the known bug file: ignore it
            return mp
          Right ls ->
            let buginfo = interestingLineMap ls
            in return $ if Map.null buginfo
                        then mp
                        else Map.insert fpath buginfo mp
  foldM addKnownBugInfo mempty known


-- | This function checks to see if the current test being defined corresponds to
-- one of those known bugs, and if so, returns the name of the known bugs file
-- and the summary string from that file.
isKnownBug :: KnownBugs -> TS.Sweets -> TS.Expectation -> VersionCheck
           -> Maybe (FilePath, String)
isKnownBug knownBugs sweet _expct llvmver =
  let matchOf (_,km) = and (uncurry isMatch <$> Map.assocs km)
      isMatch = \case
        "rootMatchName:" -> (TS.rootMatchName sweet `elem`)
        "llvmver:" -> case vcVersioning llvmver ^? (_Right . major) of
                        Just v -> (show v `elem`)
                        Nothing -> const False
        _ -> const True
      found = find matchOf $ Map.assocs knownBugs
      msg f = case fromMaybe [] (knownBugs ! f !? "summary:") of
                [] -> "this is a known bug"
                (h:_) -> h
      getSummary (f,_) = (f, msg f)
  in getSummary <$> found
