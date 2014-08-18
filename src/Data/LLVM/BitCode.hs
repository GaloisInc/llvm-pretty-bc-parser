module Data.LLVM.BitCode (
    -- * Bitcode Parsing
    parseBitCode,     parseBitCodeFromFile
  , parseBitCodeLazy, parseBitCodeLazyFromFile

    -- * Re-exported
  , Error(..), formatError
  ) where

import Data.LLVM.BitCode.Bitstream
    (Bitstream,parseBitCodeBitstream,parseBitCodeBitstreamLazy)
import Data.LLVM.BitCode.IR (parseModule)
import Data.LLVM.BitCode.Parse (runParse,badRefError,Error(..),formatError)
import Text.LLVM.AST (Module)

import Control.Monad ((<=<))
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

parseBitCode :: S.ByteString -> IO (Either Error Module)
parseBitCode  = parseBitstream . parseBitCodeBitstream

parseBitCodeFromFile :: FilePath -> IO (Either Error Module)
parseBitCodeFromFile  = parseBitCode <=< S.readFile

parseBitCodeLazy :: L.ByteString -> IO (Either Error Module)
parseBitCodeLazy  = parseBitstream . parseBitCodeBitstreamLazy

parseBitCodeLazyFromFile :: FilePath -> IO (Either Error Module)
parseBitCodeLazyFromFile  = parseBitCodeLazy <=< L.readFile

parseBitstream :: Either String Bitstream -> IO (Either Error Module)
parseBitstream e = case e of
  Left err   -> mkError ["Bitstream"] err
  Right bits -> X.handle (return . Left . badRefError)
                         (X.evaluate (runParse (parseModule bits)))
  where
  mkError cxt msg = return $ Left Error
    { errMessage = msg
    , errContext = cxt
    }
