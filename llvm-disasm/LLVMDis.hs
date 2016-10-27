
import Data.LLVM.BitCode (parseBitCode, formatError)
import Data.LLVM.CFG (buildCFG, CFG(..), blockId)
import Text.LLVM.AST (defBody, modDefines)
import Text.LLVM.PP (ppLLVM, ppModule)
import Text.PrettyPrint (Style(..), renderStyle, style)

import Control.Monad (when)
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Graph.Inductive.Dot (fglToDotString, showDot)
import Data.List (partition)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure)
import System.IO (stderr,hPutStrLn)
import qualified Data.ByteString as S

main :: IO ()
main  = do
  args <- getArgs
  let (doCFG, files) = partition (== "-cfg") args
  when (null files) (printUsage >> exitFailure)
  mapM_ (disasm (not $ null doCFG)) files

printUsage :: IO ()
printUsage  = do
  name <- getProgName
  putStrLn ("Usage: " ++ name ++ " [-cfg] { file.bc }+")

disasm :: Bool -> FilePath -> IO ()
disasm doCFG file = do
  putStrLn (replicate 80 ';' ++ "\n")
  putStrLn ("; " ++ file)
  e <- parseBitCode =<< S.readFile file
  case e of

    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure

    Right m  -> do
      let s = style { lineLength = maxBound, ribbonsPerLine = 1.0 }
      putStrLn (renderStyle s (ppLLVM (ppModule m)))
      when doCFG $ do
        let cfgs  = map (buildCFG . defBody) $ modDefines m
            fixup = nmap (show . blockId) . emap (const "")
        mapM_ (putStrLn . showDot . fglToDotString . fixup . cfgGraph) cfgs
