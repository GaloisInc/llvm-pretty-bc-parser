{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.LLVM.BitCode (parseBitCodeLazyFromFile,Error(..),formatError)
import qualified Text.LLVM.AST as AST
import           Text.LLVM.PP (ppLLVM,ppModule)

import qualified Control.Exception as X
import           Control.Lens ( (^?), _Right )
import           Control.Monad ( unless, when )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as L
import           Data.Char (ord,isLetter,isSpace,chr)
import           Data.Functor ( (<&>) )
import           Data.Generics (everywhere, mkT) -- SYB
import           Data.List ( isInfixOf, sort )
import           Data.Proxy ( Proxy(..) )
import           Data.String.Interpolate
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Versions (Versioning, versioning, prettyV, major)
import qualified GHC.IO.Exception as GE
import qualified Options.Applicative as OA
import qualified Prettyprinter as PP
import qualified Prettyprinter.Util as PPU
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Exit (ExitCode(..), exitFailure, exitSuccess)
import           System.FilePath ( (<.>) )
import           System.IO (openBinaryTempFile,hClose,openTempFile,hPutStrLn)
import qualified System.IO as IO (stderr)
import qualified System.Process as Proc
import           Test.Tasty
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
 parsing bitcode properly."
      |]
  , ""
  , block [iii|
 The method it uses is to start with a known .ll file (LLVM text assembly
 format) and assemble it to LLVM bitcode (via llvm-as from the LLVM tool
 suite). Then the test will use both llvm-dis (from the LLVM tool suite)
 and llvm-disasm (from this package, via direct library calls) to convert
 that bitcode back into the .ll text format, and also (for the latter) into
 an AST representation.
       |]
  , ""
      , "          +-------------[2nd cycle]------------+"
      , "          |                                    |"
      , "          v                                    |"
      , " .ll --[llvm-as]--> .bc ---[llvm-dis]--> .ll   |"
      , "                         `-[llvm-disasm]---> .ll"
      , "                                         `-> .AST"
      , "                                               |"
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
           package is more permissive. Opaque pointers will become
           the standard in LLVM 16.
      |]
  , ""
  , PP.indent 2 $ PP.hang 2 $ block
    [iii|* Metadata information between the two AST text formats is *not*
           compared (and is actually discarded before serialization (resetting
           all indices to zero).  This is because metadata indexes are
           dynamically generated and therefore unstable.
        |]
  , ""
  , PP.indent 2 $ PP.hang 2 $ block
    [iii|* If the --roundtrip-disabled is specified on the command line
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

disasmTestIngredients :: [TR.Ingredient]
disasmTestIngredients =
  includingOptions [ TO.Option (Proxy @LLVMAs)
                   , TO.Option (Proxy @LLVMDis)
                   , TO.Option (Proxy @Roundtrip)
                   , TO.Option (Proxy @Keep)
                   ] :
  TS.sugarIngredients [cube]
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
  OA.execParser $
    OA.info (OA.helper <*> disasmOptParser)
    ( OA.fullDesc
      <> OA.header "llvm-pretty-bc-parser disassembly test suite"
      <> OA.footerDoc (Just $ PP.align $ descr)
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

-- Determine which version of an LLVM tool will be used for these tests (if
-- possible).  Uses partial 'head' but this is just tests, and failure is
-- captured.
getLLVMToolVersion :: String -> FilePath -> IO VersionCheck
getLLVMToolVersion toolName toolPath = do
  let isVerLine = isInfixOf "LLVM version"
      dropLetter = dropWhile (all isLetter)
      getVer (Right inp) =
        -- example inp: "LLVM version 10.0.1" or "clang version 11.1.0"
        case filter isVerLine $ lines inp of
          [] -> "NO VERSION IDENTIFIED FOR " <> toolName
          (l:_) -> case dropLetter $ words l of
            [] -> toolName <> " VERSION NOT PARSED: " <> l
            (v:_) -> v
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
  tests <- TS.withSugarGroups sweets testGroup $ \s _ e -> runTest s e
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
                                           , "pre-llvm15"
                                           ])
                     ]
    -- Somewhat unusually for tasty-sugar, we make the expectedSuffix the same
    -- as the rootName suffix. This is because we are comparing the contents of
    -- each .ll file against *itself* after parsing it with
    -- llvm-pretty-bc-parser, pretty-printing it with llvm-pretty, and
    -- then normalizing it. As such, each .ll file acts as its own golden file.
  , TS.expectedSuffix = "ll"
  , TS.sweetAdjuster = \cb swts -> do
      -- Performed ranged-matching of the llvm-range parameter against the
      -- version of llvm (reported by llvm-as) to filter the tasty-sugar
      -- expectations.
      disasmOpts <- liftIO parseCmdLine
      llvmver <- liftIO $ getLLVMAsVersion $ TO.lookupOption disasmOpts
      ss <- TS.rangedParamAdjuster "llvm-range"
            (readMaybe . drop (length ("pre-llvm" :: String)))
            (<)
            (vcVersioning llvmver ^? (_Right . major))
            cb swts
      -- In addition, this is a round-trip test (assemble + disassemble) where
      -- the rootname is the same as the expected name.  Filter out any
      -- expectations that don't match the root name.
      -- (e.g. remove: root=poison.ll with exp=poison.pre-llvm12.ll).
      let rootExpSame s e = TS.rootFile s == TS.expectedFile e
      return $ ss
        <&> \s -> s { TS.expected = filter (rootExpSame s) $ TS.expected s }
  }

-- | A test failure.
data TestFailure
  = ParseError Error -- ^ A parser failure.
    deriving (Typeable,Show)

instance X.Exception TestFailure

-- | Attempt to compare the assembly generated by llvm-pretty and llvm-dis.
runTest :: TS.Sweets -> TS.Expectation -> IO [TestTree]
runTest sweet expct
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
      let m' = AST.fixupOpaquePtrs m
      parsed <- printToTempFile "ll" (show (ppLLVM (ppModule m')))
      -- stripComments _keep parsed
      if roundtrip
      then do
        tmp2 <- printToTempFile "ast" (ppShow (normalizeModule m'))
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
