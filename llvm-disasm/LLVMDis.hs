{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}

import Data.LLVM.BitCode (parseBitCode, formatError)
import Data.LLVM.CFG (buildCFG, CFG(..), blockId)
import Text.LLVM.AST (defBody, modDefines,Module)
import Text.LLVM.PP (ppLLVM35, ppLLVM36, ppLLVM37, ppLLVM38, ppModule)
import Text.PrettyPrint (Style(..), renderStyle, style)

import Control.Monad (when)
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Graph.Inductive.Dot (fglToDotString, showDot)
import Data.Monoid (mconcat, Endo(..))
import Text.Show.Pretty (pPrint)
import System.Console.GetOpt
  (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr,hPutStrLn)
import qualified Data.ByteString as S

data Options = Options {
    optLLVMVersion :: String
  , optDoCFG       :: Bool
  , optAST         :: Bool
  , optHelp        :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optLLVMVersion = "3.8"
  , optDoCFG       = False
  , optAST         = False
  , optHelp        = False
  }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "" ["llvm-version"] (ReqArg setLLVMVersion "VERSION")
    "print assembly compatible with this LLVM version (e.g., 3.8)"
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
     let banner = "Usage: " ++ prog ++ " [OPTIONS]"
     putStrLn (usageInfo (unlines (errs ++ [banner])) options)

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
  e <- parseBitCode =<< S.readFile file
  case e of

    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure

    Right m  -> do
        if optAST opts
          then pPrint m
          else renderLLVM opts m

renderLLVM :: Options -> Module -> IO ()
renderLLVM opts m = do
  let s         = style { lineLength = maxBound, ribbonsPerLine = 1.0 }
  let v         = optLLVMVersion opts
  let putRender = putStrLn . renderStyle s
  if -- try the 3.5 style for 3.4
     | v == "3.4"            -> putRender (ppLLVM35 (ppModule m))
     | v == "3.5"            -> putRender (ppLLVM35 (ppModule m))
     | v == "3.6"            -> putRender (ppLLVM36 (ppModule m))
     | v == "3.7"            -> putRender (ppLLVM37 (ppModule m))
     | v == "3.8"            -> putRender (ppLLVM38 (ppModule m))
     -- try the 3.8 style for 3.9-7.0
     | v == "3.9"            -> putRender (ppLLVM38 (ppModule m))
     | v `elem` ["4", "4.0"] -> putRender (ppLLVM38 (ppModule m))
     | v `elem` ["5", "5.0"] -> putRender (ppLLVM38 (ppModule m))
     | v `elem` ["6", "6.0"] -> putRender (ppLLVM38 (ppModule m))
     | v `elem` ["7", "7.0"] -> putRender (ppLLVM38 (ppModule m))
     | v `elem` ["8", "8.0"] -> putRender (ppLLVM38 (ppModule m))
     | v `elem` ["9", "9.0"] -> putRender (ppLLVM38 (ppModule m))
     | otherwise -> printUsage ["unsupported LLVM version: " ++ v] >> exitFailure
  when (optDoCFG opts) $ do
    let cfgs  = map (buildCFG . defBody) $ modDefines m
        fixup = nmap (show . blockId) . emap (const "")
    mapM_ (putStrLn . showDot . fglToDotString . fixup . cfgGraph) cfgs
