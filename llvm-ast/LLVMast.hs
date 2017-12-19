{-# LANGUAGE ImplicitParams #-}
import Data.LLVM.BitCode (parseBitCode, formatError)

import Control.Monad (when)
import Data.Monoid (mconcat, Endo(..))
import System.Console.GetOpt
  (ArgOrder(..), ArgDescr(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr,hPutStrLn)
import Text.Show.Pretty
import qualified Data.ByteString as S

data Options = Options {
    optHelp        :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options {
    optHelp        = False
  }

options :: [OptDescr (Endo Options)]
options  =
  [ Option "h" ["help"] (NoArg setHelp)
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

setHelp :: Endo Options
setHelp = Endo (\opt -> opt { optHelp = True })

main :: IO ()
main  = do
  (opts, files) <- getOptions
  mapM_ (ppAST opts) files

ppAST :: Options -> [Char] -> IO ()
ppAST opts file = do
  putStrLn (replicate 80 '-' ++ "\n")
  putStrLn ("-- " ++ file)
  e <- parseBitCode =<< S.readFile file
  case e of

    Left err -> do
      hPutStrLn stderr (formatError err)
      exitFailure

    Right m  -> do
        putStrLn $ ppShow m
