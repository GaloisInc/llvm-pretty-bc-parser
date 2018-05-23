{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.LLVM.BitCode.IR where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.BitString
import Data.LLVM.BitCode.IR.Blocks
import Data.LLVM.BitCode.IR.Module (parseModuleBlock)
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST

import Control.Monad ((<=<),unless,forM_)
import Data.Monoid (mappend)
import Data.Word (Word16)


-- Module Parsing --------------------------------------------------------------

-- | The magic number that identifies a @Bitstream@ structure as LLVM IR.
llvmIrMagic :: Word16
llvmIrMagic  = fromBitString (toBitString 8 0xc0 `mappend` toBitString 8 0xde)

-- | Parse an LLVM Module out of a Bitstream object.
parseModule :: Bitstream -> Parse Module
parseModule Bitstream { bsAppMagic, bsEntries } = label "Bitstream" $ do
  unless (bsAppMagic == llvmIrMagic) (fail "Bitstream is not an llvm-ir")
  parseTopLevel bsEntries

findTables :: [Entry] -> Parse ()
findTables es = forM_ es $ \e ->
  case e of
    (strtabBlockId -> Just [ abbrevDef -> Just d
                           , abbrev -> Just (fromAbbrev -> Just r)
                           ]) -> do
      st <- mkStrtab <$> parseField r 0 fieldBlob
      setStringTable st
    --(symtabBlockId -> Just _) -> fail "Found symbol table."
    _ -> return ()

-- | The only top-level block that we parse currently is the module block. The
-- Identification block that's introduced in 3.8 is ignored. In the future, it
-- might be advantageous to parse it, as it could include information that would
-- aid error reporting.
parseTopLevel :: [Entry] -> Parse Module

parseTopLevel ((moduleBlockId -> Just blockEntries) : rest) = do
  findTables rest
  parseModuleBlock blockEntries

parseTopLevel (_ : rest) =
  parseTopLevel rest

parseTopLevel [] =
  fail "MODULE_BLOCK missing"
