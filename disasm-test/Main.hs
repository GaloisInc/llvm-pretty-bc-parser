{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.LLVM.BitCode (parseBitCodeLazyFromFile,Error(..),formatError)
import qualified Text.LLVM.AST as AST
import           Text.LLVM.PP (ppLLVM,ppModule)

import qualified Control.Exception as X
import           Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import           Data.Char (ord,isSpace,chr)
import           Data.Generics (everywhere, mkT) -- SYB
import           Data.List (sort)
import           Data.Monoid ( mconcat, Endo(..) )
import           Data.Typeable (Typeable)
import           System.Console.GetOpt
           ( ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo )
import           System.Directory (getTemporaryDirectory, listDirectory, removeFile)
import           System.Environment (getArgs,getProgName)
import           System.Exit (exitFailure,exitSuccess,ExitCode(..))
import           System.FilePath ((<.>),dropExtension,takeFileName)
import           System.IO
    (openBinaryTempFile,hClose,openTempFile,hPutStrLn)
import qualified System.Process as Proc
import           Text.Show.Pretty (ppShow)


-- Option Parsing --------------------------------------------------------------

data Options = Options { optTests     :: [FilePath] -- ^ Tests
                       , optLlvmAs    :: String     -- ^ llvm-as  name
                       , optLlvmDis   :: String     -- ^ llvm-dis name
                       , optRoundtrip :: Bool
                       , optKeep      :: Bool
                       , optHelp      :: Bool
                       } deriving (Show)

-- | There are default tests because these run during @cabal test@
defaultOptions :: Options
defaultOptions  = Options { optTests     = []
                          , optLlvmAs    = "llvm-as"
                          , optLlvmDis   = "llvm-dis"
                          , optRoundtrip = False
                          , optKeep      = False
                          , optHelp      = False
                          }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "" ["with-llvm-as"] (ReqArg setLlvmAs "FILEPATH")
    "path to llvm-as"
  , Option "" ["with-llvm-dis"] (ReqArg setLlvmDis "FILEPATH")
    "path to llvm-dis"
  , Option "r" ["roundtrip"] (NoArg setRoundtrip)
    "enable roundtrip tests (AST/AST diff)"
  , Option "k" ["keep"] (NoArg setKeep)
    "keep all generated files for manual inspection"
  , Option "h" ["help"] (NoArg setHelp)
    "display this message"
  ]
  where setLlvmAs str  = Endo (\opt -> opt { optLlvmAs    = str })
        setLlvmDis str = Endo (\opt -> opt { optLlvmDis   = str })
        setRoundtrip   = Endo (\opt -> opt { optRoundtrip = True })
        setKeep        = Endo (\opt -> opt { optKeep      = True })
        setHelp        = Endo (\opt -> opt { optHelp      = True })

addTest :: String -> Endo Options
addTest test = Endo (\opt -> opt { optTests = test : optTests opt })

getOptions :: IO Options
getOptions  =
  do args <- getArgs
     case getOpt (ReturnInOrder addTest) options args of

       (fs,[],[]) -> do let opts = appEndo (mconcat fs) defaultOptions

                        when (optHelp opts) $ do printUsage []
                                                 exitSuccess

                        return opts

       (_,_,errs) -> do printUsage errs
                        exitFailure

printUsage :: [String] -> IO ()
printUsage errs =
  do prog <- getProgName
     let banner = "Usage: " ++ prog ++ " [OPTIONS] test1.ll .. testn.ll"
     putStrLn (usageInfo (unlines (errs ++ [banner])) options)


-- Test Running ----------------------------------------------------------------

-- | Run all provided tests.
main :: IO ()
main  = do
  opts <- getOptions
  -- When no tests are provided (i.e. during "cabal test"),
  -- try reading them from the test directory
  mapM_ (runTest opts) =<<
    if null (optTests opts)
    then let dir = "disasm-test/tests/"
         in map (dir++) <$> listDirectory dir
    else pure (optTests opts)

-- | A test failure.
data TestFailure
  = ParseError Error -- ^ A parser failure.
    deriving (Typeable,Show)

instance X.Exception TestFailure

-- | Attempt to compare the assembly generated by llvm-pretty and llvm-dis.
runTest :: Options -> FilePath -> IO ()
runTest opts file =
  let -- Assemble and disassemble some LLVM asm
      processLL :: FilePath -> IO (FilePath, Maybe FilePath)
      processLL f = do
        putStrLn (showString f ": ")
        X.handle logError                                $
          withFile  (generateBitCode  opts pfx f)        $ \ bc   ->
          withFile  (normalizeBitCode opts pfx bc)       $ \ norm -> do
            (parsed, ast) <- processBitCode opts pfx bc
            ignore (Proc.callProcess "diff" ["-u", norm, parsed])
            putStrLn ("successfully parsed " ++ show f)
            return (parsed, ast)
  in do
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
  where pfx   = dropExtension (takeFileName file)
        exitF l = mapM_ putStrLn l >> exitFailure
        withFile iofile f =
          X.bracket iofile (if optKeep opts then const (pure ()) else removeFile) f
        logError (ParseError msg) = do
          putStrLn "failure"
          putStrLn (unlines (map ("; " ++) (lines (formatError msg))))
          exitFailure
        diff file1 file2 = do
          (code, stdout, stderr) <-
            Proc.readCreateProcessWithExitCode (Proc.proc "diff" ["-u", file1, file2]) ""
          case code of
            ExitFailure _ -> exitF ["diff failed", stdout, stderr]
            ExitSuccess   ->
              if stdout /= "" || stderr /= ""
              then exitF ["non-empty diff", stdout, stderr]
              else mapM_ putStrLn ["success: empty diff: ", file1, file2]

-- | Assemble some llvm assembly, producing a bitcode file in /tmp.
generateBitCode :: Options -> FilePath -> FilePath -> IO FilePath
generateBitCode Options { .. } pfx file = do
  tmp    <- getTemporaryDirectory
  (bc,h) <- openBinaryTempFile tmp (pfx <.> "bc")
  hClose h
  callProc optLlvmAs ["-o", bc, file]
  return bc

-- | Use llvm-dis to parse a bitcode file, to obtain a normalized version of the
-- llvm assembly.
normalizeBitCode :: Options -> FilePath -> FilePath -> IO FilePath
normalizeBitCode Options { .. } pfx file = do
  tmp      <- getTemporaryDirectory
  (norm,h) <- openTempFile tmp (pfx ++ "llvm-dis" <.> "ll")
  hClose h
  callProc optLlvmDis ["-o", norm, file]
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
processBitCode :: Options -> FilePath -> FilePath -> IO (FilePath, Maybe FilePath)
processBitCode Options { .. } pfx file = do
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
      -- stripComments parsed
      if optRoundtrip
      then do
        tmp2 <- printToTempFile "ast" (ppShow (normalizeModule m))
        return (parsed, Just tmp2)
      else return (parsed, Nothing)

-- | Remove comments from a .ll file, stripping everything including the
-- semi-colon.
stripComments :: Options -> FilePath -> IO ()
stripComments Options { .. } path = do
  bytes <- L.readFile path
  when (not optKeep) (removeFile path)
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
