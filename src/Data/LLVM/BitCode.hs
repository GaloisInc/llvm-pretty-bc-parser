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
import Data.Sequence (Seq)

-- | Parse the contents of an LLVM bitcode file as a strict 'S.ByteString'. If
-- parsing succeeds, return the parsed 'Module'. Otherwise, return an 'Error'
-- describing what went wrong.
--
-- See also 'parseBitCodeWithWarnings' for a version that also returns any
-- warnings that arise during parsing. This function will simply discard all
-- such warnings, which is why this function is deprecated.
parseBitCode :: S.ByteString -> IO (Either Error Module)
parseBitCode =
  fmap (fmap fst) . parseBitstream . parseBitCodeBitstream
{-# DEPRECATED
      parseBitCode
      "Use parseBitCodeWithWarnings instead." #-}

-- | Load an LLVM bitcode file as a strict 'S.ByteString' and parse its
-- contents. If parsing succeeds, return the parsed 'Module'. Otherwise, return
-- an 'Error' describing what went wrong.
--
-- See also 'parseBitCodeFromFileWithWarnings' for a version that also returns
-- any warnings that arise during parsing. This function will simply discard all
-- such warnings, which is why this function is deprecated.
parseBitCodeFromFile :: FilePath -> IO (Either Error Module)
parseBitCodeFromFile =
  parseBitCode <=< S.readFile
{-# DEPRECATED
      parseBitCodeFromFile
      "Use parseBitCodeFromFileWithWarnings instead." #-}

-- | Parse the contents of an LLVM bitcode file as a lazy 'L.ByteString'. If
-- parsing succeeds, return the parsed 'Module'. Otherwise, return an 'Error'
-- describing what went wrong.
--
-- See also 'parseBitCodeLazyWithWarnings' for a version that also returns any
-- warnings that arise during parsing. This function will simply discard all
-- such warnings, which is why this function is deprecated.
parseBitCodeLazy :: L.ByteString -> IO (Either Error Module)
parseBitCodeLazy =
  fmap (fmap fst) . parseBitstream . parseBitCodeBitstreamLazy
{-# DEPRECATED
      parseBitCodeLazy
      "Use parseBitCodeLazyWithWarnings instead." #-}

-- | Load an LLVM bitcode file as a lazy 'L.ByteString' and parse its contents.
-- If parsing succeeds, return the parsed 'Module'. Otherwise, return an 'Error'
-- describing what went wrong in a 'Left' value.
--
-- See also 'parseBitCodeLazyFromFileWithWarnings' for a version that also
-- returns any warnings that arise during parsing. This function will simply
-- discard all such warnings, which is why this function is deprecated.
parseBitCodeLazyFromFile :: FilePath -> IO (Either Error Module)
parseBitCodeLazyFromFile =
  parseBitCodeLazy <=< L.readFile
{-# DEPRECATED
      parseBitCodeLazyFromFile
      "Use parseBitCodeLazyFromFileWithWarnings instead." #-}

-- | Parse the contents of an LLVM bitcode file as a strict 'S.ByteString'. If
-- parsing succeeds, return the parsed 'Module' and any 'ParseWarning's that
-- were emitted. Otherwise, return an 'Error' describing what went wrong.
--
-- See also 'parseBitCode' for a version that discards any warnings that arise
-- during parsing.
parseBitCodeWithWarnings ::
  S.ByteString -> IO (Either Error (Module, Seq ParseWarning))
parseBitCodeWithWarnings =
  parseBitstream . parseBitCodeBitstream

-- | Load an LLVM bitcode file as a strict 'S.ByteString' and parse its
-- contents. If parsing succeeds, return the parsed 'Module' and any
-- 'ParseWarnings' that were emitted. Otherwise, return an 'Error' describing
-- what went wrong.
--
-- See also 'parseBitCodeFromFile' for a version that discards any warnings that
-- arise during parsing.
parseBitCodeFromFileWithWarnings ::
  FilePath -> IO (Either Error (Module, Seq ParseWarning))
parseBitCodeFromFileWithWarnings =
  parseBitCodeWithWarnings <=< S.readFile

-- | Parse the contents of an LLVM bitcode file as a lazy 'L.ByteString'. If
-- parsing succeeds, return the parsed 'Module' and any 'ParseWarning's that
-- were emitted. Otherwise, return an 'Error' describing what went wrong.
--
-- See also 'parseBitCodeLazy' for a version that discards any warnings that
-- arise during parsing.
parseBitCodeLazyWithWarnings ::
  L.ByteString -> IO (Either Error (Module, Seq ParseWarning))
parseBitCodeLazyWithWarnings =
  parseBitstream . parseBitCodeBitstreamLazy

-- | Load an LLVM bitcode file as a lazy 'L.ByteString' and parse its contents.
-- If parsing succeeds, return the parsed 'Module' and any 'ParseWarnings' that
-- were emitted. Otherwise, return an 'Error' describing what went wrong.
--
-- See also 'parseBitCodeLazyFromFile' for a version that discards any warnings
-- that arise during parsing.
parseBitCodeLazyFromFileWithWarnings ::
  FilePath -> IO (Either Error (Module, Seq ParseWarning))
parseBitCodeLazyFromFileWithWarnings =
  parseBitCodeLazyWithWarnings <=< L.readFile

parseBitstream ::
  Either String Bitstream -> IO (Either Error (Module, Seq ParseWarning))
parseBitstream e = case e of
  Left err   -> mkError ["Bitstream"] err
  Right bits -> do
    res <- X.handle (return . Left . badRefError)
                    (X.evaluate (runParse (parseModule bits)))
    pure $ fmap (\(m, st) -> (m, psWarnings st)) res
  where
  mkError cxt msg = return $ Left Error
    { errMessage = msg
    , errContext = cxt
    }
