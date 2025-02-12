import Data.LLVM.BitCode (parseBitCodeWithWarnings, formatError, ppParseWarnings)
import Data.LLVM.CFG (buildCFG, CFG(..), blockId)
import Text.LLVM.AST (defBody, modDefines,Module)
import Text.LLVM.PP (ppLLVM, ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38, llvmPP, llvmVlatest)

import Control.Monad (unless, when)
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Graph.Inductive.Dot (fglToDotString, showDot)
import Data.Monoid (Endo(..))
import Text.PrettyPrint (Style(..), renderStyle, style)
import Text.Read (readMaybe)
import Text.Show.Pretty (pPrint)
import System.Console.GetOpt
  (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr,hPrint,hPutStrLn)
import qualified Data.ByteString as S

data Options = Options {
    optLLVMVersion :: String
  , optDoCFG       :: Bool
  , optAST         :: Bool
  , optHelp        :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optLLVMVersion = show llvmVlatest
  , optDoCFG       = False
  , optAST         = False
  , optHelp        = False
  }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "" ["llvm-version"] (ReqArg setLLVMVersion "VERSION")
    "output for LLVM version (e.g., 3.5, 3.6. 3.7, 3.8, 4, 5, 6, ...)."
  , Option "" ["cfg"] (NoArg setDoCFG)
    "output CFG in graphviz format"
  , Option "" ["ast"] (NoArg setAST)
    "output the Haskell AST instead"
  , Option "h" ["help"] (NoArg setHelp)
    "display this message"
  ]

getOptions :: IO (Options, [FilePath])
getOptions =
  do args <- getArgs
     case getOpt RequireOrder options args of
       (fs,files@(_:_),[]) -> do
         let opts = appEndo (mconcat fs) defaultOptions
         when (optHelp opts) $
           printUsage [] >> exitSuccess
         return (opts,files)
       (_,_,errs) -> do printUsage errs
                        exitFailure

printUsage :: [String] -> IO ()
printUsage errs =
  do prog <- getProgName
     let banner = [ "Usage: " ++ prog ++ " [OPTIONS]"
                  , ""
                  , "  Converts LLVM bitcode format (.bc) to LLVM text form (.ll) on stdout."
                  , "  Supports LLVM versions 3.4 through " <> show llvmVlatest <> "."
                  , ""
                  , "  Comparable to the llvm-dis tool from LLVM (which only supports"
                  , "  the *current* version) but writes to stdout instead of a file."
                  ]

     putStrLn (usageInfo (unlines (errs ++ banner)) options)

setLLVMVersion :: String -> Endo Options
setLLVMVersion str = Endo (\opt -> opt { optLLVMVersion = str })

setDoCFG :: Endo Options
setDoCFG = Endo (\opt -> opt { optDoCFG = True })

setAST :: Endo Options
setAST = Endo (\opt -> opt { optAST = True })

setHelp :: Endo Options
setHelp = Endo (\opt -> opt { optHelp = True })

main :: IO ()
main  = do
  (opts, files) <- getOptions
  mapM_ (disasm opts) files

disasm :: Options -> [Char] -> IO ()
disasm opts file = do
  putStrLn (replicate 80 ';' ++ "\n")
  putStrLn ("; " ++ file)
  e <- parseBitCodeWithWarnings =<< S.readFile file
  case e of

    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure

    Right (m, warnings) -> do
        unless (null warnings) $
          hPrint stderr $ ppParseWarnings warnings
        if optAST opts
          then pPrint m
          else renderLLVM opts m

renderLLVM :: Options -> Module -> IO ()
renderLLVM opts m = do
  let s         = style { lineLength = maxBound, ribbonsPerLine = 1.0 }
  let v         = optLLVMVersion opts
  let putRender = putStrLn . renderStyle s
  case readMaybe v :: Maybe Int of
    Just n -> putRender (ppLLVM n (llvmPP m))
    Nothing -> case readMaybe v :: Maybe Float of
                 Just 3.4 -> putRender (ppLLVM35 (llvmPP m))
                 Just 3.5 -> putRender (ppLLVM35 (llvmPP m))
                 Just 3.6 -> putRender (ppLLVM36 (llvmPP m))
                 Just 3.7 -> putRender (ppLLVM37 (llvmPP m))
                 Just 3.8 -> putRender (ppLLVM38 (llvmPP m))
                 _ -> printUsage ["unsupported LLVM version: " ++ v] >> exitFailure
  when (optDoCFG opts) $ do
    let cfgs  = map (buildCFG . defBody) $ modDefines m
        fixup = nmap (show . blockId) . emap (const "")
    mapM_ (putStrLn . showDot . fglToDotString . fixup . cfgGraph) cfgs
