{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.LLVM.BitCode.IR where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.BitString
import Data.LLVM.BitCode.IR.Module (parseModuleBlock)
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Text.LLVM.AST

import Control.Monad ((<=<),unless)
import Data.Monoid (mappend)
import Data.Word (Word16)


-- Module Parsing --------------------------------------------------------------

-- | The magic number that identifies a @Bitstream@ structure as LLVM IR.
llvmIrMagic :: Word16
llvmIrMagic  = fromBitString (toBitString 8 0xc0 `mappend` toBitString 8 0xde)

-- | Block selector for the top-level module block.
moduleBlock :: Match Entry [Entry]
moduleBlock  = fmap blockEntries . hasBlockId 8 <=< block

-- | Parse an LLVM Module out of a Bitstream object.
parseModule :: Bitstream -> Parse Module
parseModule Bitstream { bsAppMagic, bsEntries } = label "Bitstream" $ do
  unless (bsAppMagic == llvmIrMagic) (fail "Bitstream is not an llvm-ir")
  parseTopLevel bsEntries


-- | The only top-level block that we parse currently is the module block. The
-- Identification block that's introduced in 3.8 is ignored. In the future, it
-- might be advantageous to parse it, as it could include information that would
-- aid error reporting.
parseTopLevel :: [Entry] -> Parse Module

parseTopLevel (EntryBlock Block { blockId = 8, blockEntries } : _) =
  parseModuleBlock blockEntries

parseTopLevel (_ : rest) =
  parseTopLevel rest

parseTopLevel [] =
  fail "MODULE_BLOCK missing"
