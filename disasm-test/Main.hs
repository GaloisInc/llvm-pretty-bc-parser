{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.LLVM.BitCode (parseBitCodeLazyFromFile,Error(..),formatError)
import qualified Text.LLVM.AST as AST
import           Text.LLVM.PP (ppLLVM,ppModule)

import qualified Control.Exception as EX
import           Control.Lens ( (^?), _Right )
import           Control.Monad ( foldM, unless, when )
import qualified Control.Monad.Catch as X
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans.State
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy.UTF8 ( toString )
import           Data.Char (ord,isLetter,isSpace,chr)
import           Data.Generics (everywhere, mkT) -- SYB
import           Data.List ( find, isInfixOf, isPrefixOf, nub, sort )
import           Data.Proxy ( Proxy(..) )
import           Data.String.Interpolate
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Versions (Versioning, versioning, prettyV, major)
import qualified GHC.IO.Exception as GE
import qualified Options.Applicative as OA
import qualified Prettyprinter as PP
import qualified Prettyprinter.Util as PPU
import qualified System.Console.Terminal.Size as Term
import           System.Directory ( doesFileExist, getTemporaryDirectory
                                  , listDirectory
                                  , removeFile )
import           System.Exit (ExitCode(..), exitFailure, exitSuccess)
import           System.FilePath ( (</>), (<.>) )
import           System.IO (openBinaryTempFile,hClose,openTempFile,hPutStrLn)
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
 The compiler test method starts with a known .c file and uses clang to directly
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
  , " .c --[clang]--------+                         |"
  , "                                             [show]"
  , "                                               |"
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
  ]

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

disasmTestIngredients :: [TR.Ingredient]
disasmTestIngredients =
  includingOptions [ TO.Option (Proxy @LLVMAs)
                   , TO.Option (Proxy @LLVMDis)
                   , TO.Option (Proxy @Clang)
                   , TO.Option (Proxy @Roundtrip)
                   , TO.Option (Proxy @Keep)
                   , TO.Option (Proxy @Details)
                   ] :
  TS.sugarIngredients [ assemblyCube, compilerCube ]
  <> defaultIngredients

parseCmdLine :: IO TO.OptionSet
parseCmdLine = do
  TR.installSignalHandlers
  let disasmOptDescrs = TO.uniqueOptionDescriptions $
        TR.coreOptions ++
        TS.sugarOptions ++
        TR.ingredientsOptions disasmTestIngredients
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

  sweets1 <- TS.findSugar assemblyCube
  sweets2 <- TS.findSugar compilerCube
  atests <- TS.withSugarGroups sweets1 testGroup $ \s _ e -> runAssemblyTest s e
  ctests <- TS.withSugarGroups sweets2 testGroup $ \s _ e -> runCompileTest s e
  let tests = atests <> ctests
  case TR.tryIngredients
         disasmTestIngredients
         disasmOpts
         (testGroup "Disassembly tests"
          [ testGroup (showVC llvmAsVC) atests
          , testGroup (showVC clangVC) ctests
          ]) of
    Nothing ->
      hPutStrLn IO.stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

  defaultMainWithIngredients disasmTestIngredients $
    testGroup "Disassembly tests" tests

----------------------------------------------------------------------
-- Assembly/disassembly tests

assemblyCube :: TS.CUBE
assemblyCube = TS.mkCUBE
  { TS.inputDirs = ["disasm-test/tests"]
  , TS.rootName = "*.ll"
  , TS.separators = "."
  , TS.validParams = [ ("llvm-range", Just [ "recent-llvm"
                                           , "pre-llvm12"
                                           , "pre-llvm13"
                                           , "pre-llvm14"
                                           , "pre-llvm15"
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
      in fmap (fmap addExpFilter) . rangeMatch cb
  }


rangeMatch :: MonadIO m => TS.CUBE -> [TS.Sweets] -> m [TS.Sweets]
rangeMatch cb swts = do
  -- Perform ranged-matching of the llvm-range parameter against the
  -- version of llvm (reported by llvm-as) to filter the tasty-sugar
  -- expectations.
  disasmOpts <- liftIO $ parseCmdLine
  llvmver <- liftIO $ getLLVMAsVersion $ TO.lookupOption disasmOpts
  TS.rangedParamAdjuster "llvm-range"
    (readMaybe . drop (length ("pre-llvm" :: String)))
    (<)
    (vcVersioning llvmver ^? (_Right . major))
    cb swts


-- | Returns true if this particular test should be skipped, which is signalled
-- by the expected file contents starting with "SKIP_TEST".  For test cases that
-- require a minimum LLVM version, this technique is used to prevent running the
-- test on older LLVM versions.
skipTest :: TS.Expectation -> IO Bool
skipTest expct =
  ("SKIP_TEST" `L.isPrefixOf`) <$> L.readFile (TS.expectedFile expct)

-- | There is a directory containing files which describe known bugs (one per
-- file).  This function checks to see if the current test being defined
-- corresponds to one of those known bugs, and if so, returns the name of the
-- known bugs file and the summary string from that file.
isKnownBug :: TS.Sweets -> IO (Maybe (FilePath, String))
isKnownBug sweet = do
  let kbdir = "disasm-test/known_bugs"
  known <- listDirectory kbdir
  let isSummaryLine = ("summary: " `isPrefixOf`)
  let isMatchLine l = and [ "rootMatchName: " `isPrefixOf` l
                          , TS.rootMatchName sweet `elem` words l
                          ]
  let checkMatch = \case
        m@(Just _) -> const $ return m
        Nothing -> \f -> do lse <- X.try (lines . toString <$> L.readFile (kbdir </> f))
                            case lse of
                              Left (_e :: IOError) ->
                                -- just ignore this known_bugs file
                                return Nothing
                              Right ls ->
                                let matches = not $ null $ filter isMatchLine ls
                                in if matches
                                   then let summary =
                                              case find isSummaryLine ls of
                                                Nothing -> "this is a known bug"
                                                Just s -> s
                                        in return $ Just (kbdir </> f, summary)
                                   else return Nothing
  foldM checkMatch Nothing known


-- | Attempt to compare the assembly generated by llvm-pretty and llvm-dis.
runAssemblyTest :: TS.Sweets -> TS.Expectation -> IO [TestTree]
runAssemblyTest sweet expct
  = do shouldSkip <- skipTest expct
       knownBug <- isKnownBug sweet
       let tmod = if shouldSkip
                  then ignoreTestBecause "not valid for this LLVM version"
                  else case knownBug of
                         Just (from, why) ->
                           expectFailBecause $ why <> " [see " <> from <> "]"
                         Nothing -> id
       let pfx = TS.rootBaseName sweet
       return $ (:[]) $ tmod
         $ testCaseM pfx
         $ with2Files (processLL pfx $ TS.rootFile sweet)
         $ \(parsed1, ast) ->
             case ast of
               Nothing   -> return ()
               Just ast1 ->
                 -- Re-assemble and re-disassemble
                 with2Files (processLL pfx parsed1) $ \(_, Just ast2) -> do
                 diffCmp ast1 ast2 -- Ensure that the ASTs match

                 -- Ensure that the disassembled files match.  This is usually
                 -- too strict (and doesn't really provide more info).  We
                 -- normalize the AST (see below) to ensure that the ASTs match
                 -- modulo metadata numbering, but the equivalent isn't possible
                 -- for the assembly: we need llvm-as to be able to re-assemble
                 -- it.
                 --
                 -- diffCmp parsed1 parsed2


diffCmp :: FilePath -> FilePath -> TestM ()
diffCmp file1 file2 = do
  let assertF = liftIO . assertFailure . unlines
  (code, stdout, stderr) <- liftIO $
    Proc.readCreateProcessWithExitCode (Proc.proc "diff" ["-u", file1, file2]) ""
  case code of
    ExitFailure _ -> assertF ["diff failed", stdout, stderr]
    ExitSuccess   ->
      if stdout /= "" || stderr /= ""
      then assertF ["non-empty diff", stdout, stderr]
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
  withFile (disasmBitCode pfx bc) $ \ norm -> do
    (parsed, ast) <- processBitCode pfx bc
    Details dets <- gets showDetails
    when dets $ liftIO $ do
      -- Informationally display if there are differences between the llvm-dis
      -- and llvm-disasm outputs, but no error if they differ.
      ignore (Proc.callProcess "diff" ["-u", norm, parsed])
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
-- The compilerCube uses .c files as the input, and .ll files for the expected
-- output.  The assemblyCube uses the .ll file as both input and output.  The
-- actual testing done is very similar, and the assemblyCube always generates a
-- superset of the compilerCube tests (i.e. when no .c file is present).

compilerCube :: TS.CUBE
compilerCube = assemblyCube
               { TS.rootName = "*.c"
               , TS.sweetAdjuster = rangeMatch
               }


runCompileTest :: TS.Sweets -> TS.Expectation -> IO [TestTree]
runCompileTest sweet expct = do
  shouldSkip <- skipTest expct
  knownBug <- isKnownBug sweet
  let tmod = if shouldSkip
             then ignoreTestBecause "not valid for this LLVM version"
             else case knownBug of
                    Just (from, why) ->
                      expectFailBecause $ why <> " [see " <> from <> "]"
                    Nothing -> id
  let pfx = TS.rootBaseName sweet
  return $ (:[]) $ tmod
    $ testCaseM pfx
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
                $ \(_, Just ast2) -> diffCmp ast1 ast2
                  -- .ll files are not compared; see runAssemblyTest for details.


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
                           }

type TestM a = StateT TestState IO a

testCaseM :: FilePath -> TestM () -> TestTree
testCaseM pfx ops =
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
                                           })


-- | Assemble some llvm assembly, producing a bitcode file in /tmp.
assembleToBitCode :: FilePath -> FilePath -> TestM FilePath
assembleToBitCode pfx file = do
  tmp <- liftIO getTemporaryDirectory
  LLVMAs asm <- gets llvmAs
  X.bracketOnError
    (liftIO $ openBinaryTempFile tmp (pfx <.> "bc"))
    (\(bc,_) -> do exists <- liftIO $ doesFileExist bc
                   when exists $ rmFile bc
    )
    $ \(bc,h) ->
        do liftIO $ hClose h
           callProc asm ["-o", bc, file]
           return bc

-- | Compile a C or C++ source, producing a bitcode file in /tmp.
compileToBitCode :: FilePath -> FilePath -> TestM FilePath
compileToBitCode pfx file = do
  tmp <- liftIO getTemporaryDirectory
  Clang comp <- gets clang
  X.bracketOnError
    (liftIO $ openBinaryTempFile tmp (pfx <.> "bc"))
    (\(bc,_) -> do exists <- liftIO $ doesFileExist bc
                   when exists $ rmFile bc
    )
    $ \(bc,h) ->
        do liftIO $ hClose h
           callProc comp ["-c", "-emit-llvm", "-O0", "-g", "-o", bc, file]
           return bc

-- | Use llvm-dis to parse a bitcode file, to obtain a normalized version of the
-- llvm assembly.
disasmBitCode :: FilePath -> FilePath -> TestM FilePath
disasmBitCode pfx file = do
  tmp <- liftIO $ getTemporaryDirectory
  LLVMDis dis <- gets llvmDis
  X.bracketOnError
    (liftIO $ openTempFile tmp (pfx ++ "llvm-dis" <.> "ll"))
    (\(norm,_) -> do exists <- liftIO $ doesFileExist norm
                     when exists $ rmFile norm
    )
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
  let handler :: X.SomeException -> IO (Either Error AST.Module)
      handler se = return (Left (Error [] (show se)))
      printToTempFile sufx stuff = do
        tmp        <- getTemporaryDirectory
        (parsed,h) <- openTempFile tmp (pfx ++ "llvm-disasm" <.> sufx)
        hPutStrLn h stuff
        hClose h
        return parsed
  e <- liftIO $ parseBitCodeLazyFromFile file `X.catch` handler
  case e of
    Left err -> X.throwM (ParseError err)
    Right m  -> do
      let m' = AST.fixupOpaquePtrs m
      postParseTests m'
      parsed <- liftIO $ printToTempFile "ll" (show (ppLLVM (ppModule m')))
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
                  $ do Details dets <- gets showDetails
                       when dets $ liftIO $ putStrLn $ "## Removing " <> tmp
                       liftIO $ removeFile tmp
