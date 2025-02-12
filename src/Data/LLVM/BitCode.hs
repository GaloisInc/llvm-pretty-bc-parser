{-# LANGUAGE OverloadedStrings #-}

module Data.LLVM.BitCode (
    -- * Bitcode Parsing
    -- ** Without 'ParseWarning's
    parseBitCode,     parseBitCodeFromFile
  , parseBitCodeLazy, parseBitCodeLazyFromFile
    -- ** With 'ParseWarning's
  , parseBitCodeWithWarnings,     parseBitCodeFromFileWithWarnings
  , parseBitCodeLazyWithWarnings, parseBitCodeLazyFromFileWithWarnings

    -- * Re-exported
  , Error(..), formatError
  , ParseWarning(..), MetadataRecordSizeRange(..)
  , ppParseWarnings, ppParseWarning
  ) where

import Data.LLVM.BitCode.Bitstream
    (Bitstream,parseBitCodeBitstream,parseBitCodeBitstreamLazy)
import Data.LLVM.BitCode.IR (parseModule)
import Data.LLVM.BitCode.Parse (runParse,badRefError,Error(..),formatError,
                                MetadataRecordSizeRange(..),ParseWarning(..),
                                ParseState(..),ppParseWarning,ppParseWarnings)
import Text.LLVM.AST (Module)

import Control.Monad ((<=<))
import qualified Control.Exception as X
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Foldable (toList)

-- | Parse the contents of an LLVM bitcode file as a strict 'S.ByteString'. If
-- parsing succeeds, return the parsed 'Module' in a 'Right' value. Otherwise,
-- return an 'Error' describing what went wrong in a 'Left' value.
--
-- See also 'parseBitCodeWithWarnings' for a version that also returns any
-- warnings that arise during parsing.
parseBitCode :: S.ByteString -> IO (Either Error Module)
parseBitCode =
  fmap (fmap fst) . parseBitstream . parseBitCodeBitstream

-- | Load an LLVM bitcode file as a strict 'S.ByteString' and parse its
-- contents. If parsing succeeds, return the parsed 'Module' in a 'Right' value.
-- Otherwise, return an 'Error' describing what went wrong in a 'Left' value.
--
-- See also 'parseBitCodeFromFileWithWarnings' for a version that also returns
-- any warnings that arise during parsing.
parseBitCodeFromFile :: FilePath -> IO (Either Error Module)
parseBitCodeFromFile =
  parseBitCode <=< S.readFile

-- | Parse the contents of an LLVM bitcode file as a lazy 'L.ByteString'. If
-- parsing succeeds, return the parsed 'Module' in a 'Right' value. Otherwise,
-- return an 'Error' describing what went wrong in a 'Left' value.
--
-- See also 'parseBitCodeLazyWithWarnings' for a version that also returns any
-- warnings that arise during parsing.
parseBitCodeLazy :: L.ByteString -> IO (Either Error Module)
parseBitCodeLazy =
  fmap (fmap fst) . parseBitstream . parseBitCodeBitstreamLazy

-- | Load an LLVM bitcode file as a lazy 'L.ByteString' and parse its contents.
-- If parsing succeeds, return the parsed 'Module' in a 'Right' value.
-- Otherwise, return an 'Error' describing what went wrong in a 'Left' value.
--
-- See also 'parseBitCodeLazyFromFileWithWarnings' for a version that also
-- returns any warnings that arise during parsing.
parseBitCodeLazyFromFile :: FilePath -> IO (Either Error Module)
parseBitCodeLazyFromFile =
  parseBitCodeLazy <=< L.readFile

-- | Parse the contents of an LLVM bitcode file as a strict 'S.ByteString'. If
-- parsing succeeds, return the parsed 'Module' and any 'ParseWarning's that
-- were emitted in a 'Right' value. Otherwise, return an 'Error' describing what
-- went wrong in a 'Left' value.
--
-- See also 'parseBitCode' for a version that discards any warnings that arise
-- during parsing.
parseBitCodeWithWarnings ::
  S.ByteString -> IO (Either Error (Module, [ParseWarning]))
parseBitCodeWithWarnings =
  parseBitstream . parseBitCodeBitstream

-- | Load an LLVM bitcode file as a strict 'S.ByteString' and parse its
-- contents. If parsing succeeds, return the parsed 'Module' and any
-- 'ParseWarnings' that were emitted in a 'Right' value. Otherwise, return an
-- 'Error' describing what went wrong in a 'Left' value.
--
-- See also 'parseBitCodeFromFile' for a version that discards any warnings that
-- arise during parsing.
parseBitCodeFromFileWithWarnings ::
  FilePath -> IO (Either Error (Module, [ParseWarning]))
parseBitCodeFromFileWithWarnings =
  parseBitCodeWithWarnings <=< S.readFile

-- | Parse the contents of an LLVM bitcode file as a lazy 'L.ByteString'. If
-- parsing succeeds, return the parsed 'Module' and any 'ParseWarning's that
-- were emitted in a 'Right' value. Otherwise, return an 'Error' describing what
-- went wrong in a 'Left' value.
--
-- See also 'parseBitCodeLazy' for a version that discards any warnings that
-- arise during parsing.
parseBitCodeLazyWithWarnings ::
  L.ByteString -> IO (Either Error (Module, [ParseWarning]))
parseBitCodeLazyWithWarnings =
  parseBitstream . parseBitCodeBitstreamLazy

-- | Load an LLVM bitcode file as a lazy 'L.ByteString' and parse its contents.
-- If parsing succeeds, return the parsed 'Module' and any 'ParseWarnings' that
-- were emitted in a 'Right' value. Otherwise, return an 'Error' describing what
-- went wrong in a 'Left' value.
--
-- See also 'parseBitCodeLazyFromFile' for a version that discards any warnings
-- that arise during parsing.
parseBitCodeLazyFromFileWithWarnings ::
  FilePath -> IO (Either Error (Module, [ParseWarning]))
parseBitCodeLazyFromFileWithWarnings =
  parseBitCodeLazyWithWarnings <=< L.readFile

parseBitstream ::
  Either String Bitstream -> IO (Either Error (Module, [ParseWarning]))
parseBitstream e = case e of
  Left err   -> mkError ["Bitstream"] err
  Right bits -> do
    res <- X.handle (return . Left . badRefError)
                    (X.evaluate (runParse (parseModule bits)))
    pure $ fmap (\(m, st) -> (m, toList (psWarnings st))) res
  where
  mkError cxt msg = return $ Left Error
    { errMessage = msg
    , errContext = cxt
    }
