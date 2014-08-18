{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

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
parseModule bs = do
  unless (bsAppMagic bs == llvmIrMagic) (fail "Bitstream is not an llvm-ir")
  parseModuleBlock =<< match (moduleBlock <=< oneChild) (bsEntries bs)
