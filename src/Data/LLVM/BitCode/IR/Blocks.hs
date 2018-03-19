module Data.LLVM.BitCode.IR.Blocks where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Record

import Control.Monad ((<=<))


-- Generic Block Ids -----------------------------------------------------------

-- | Block info block selector.
blockInfoBlockId :: Match Entry [Entry]
blockInfoBlockId  = fmap blockEntries . hasBlockId 0 <=< block


-- Module Block Ids ------------------------------------------------------------

moduleBlockId :: Match Entry [Entry]
moduleBlockId  = fmap blockEntries . hasBlockId 8 <=< block

paramattrBlockId :: Match Entry [Entry]
paramattrBlockId  = fmap blockEntries . hasBlockId 9 <=< block

paramattrGroupBlockId :: Match Entry [Entry]
paramattrGroupBlockId  = fmap blockEntries . hasBlockId 10 <=< block

-- | Constants block selector.
constantsBlockId :: Match Entry [Entry]
constantsBlockId  = fmap blockEntries . hasBlockId 11 <=< block

-- | Function block selector.  This will succeed for function body, only.
functionBlockId :: Match Entry [Entry]
functionBlockId  = fmap blockEntries . hasBlockId 12 <=< block

-- UNUSED_ID2 (13)

-- | Value symbol table block selector.
valueSymtabBlockId :: Match Entry [Entry]
valueSymtabBlockId  = fmap blockEntries . hasBlockId 14 <=< block

-- | Metadata block selector.
metadataBlockId :: Match Entry [Entry]
metadataBlockId  = fmap blockEntries . hasBlockId 15 <=< block

-- | Metadata attachment block selector.
metadataAttachmentBlockId :: Match Entry [Entry]
metadataAttachmentBlockId  = fmap blockEntries . hasBlockId 16 <=< block

-- | TYPE_BLOCK_ID_NEW
typeBlockIdNew :: Match Entry [Entry]
typeBlockIdNew  = fmap blockEntries . hasBlockId 17 <=< block

uselistBlockId :: Match Entry [Entry]
uselistBlockId  = fmap blockEntries . hasBlockId 18 <=< block

moduleStrtabBlockId :: Match Entry [Entry]
moduleStrtabBlockId = fmap blockEntries . hasBlockId 19 <=< block

globalvalSummaryBlockId :: Match Entry [Entry]
globalvalSummaryBlockId = fmap blockEntries . hasBlockId 20 <=< block

operandBundleTagsBlockId :: Match Entry [Entry]
operandBundleTagsBlockId = fmap blockEntries . hasBlockId 21 <=< block

metadataKindBlockId :: Match Entry [Entry]
metadataKindBlockId  = fmap blockEntries . hasBlockId 22 <=< block

strtabBlockId :: Match Entry [Entry]
strtabBlockId  = fmap blockEntries . hasBlockId 23 <=< block

ltoSummaryBlockId :: Match Entry [Entry]
ltoSummaryBlockId  = fmap blockEntries . hasBlockId 24 <=< block

symtabBlockId :: Match Entry [Entry]
symtabBlockId  = fmap blockEntries . hasBlockId 25 <=< block

syncScopeNamesBlockId :: Match Entry [Entry]
syncScopeNamesBlockId  = fmap blockEntries . hasBlockId 26 <=< block

-- Module Codes ----------------------------------------------------------------

-- | MODULE_CODE_VERSION
moduleCodeVersion :: Match Entry Record
moduleCodeVersion  = hasRecordCode 1 <=< fromEntry

-- | MODULE_CODE_TRIPLE
moduleCodeTriple :: Match Entry Record
moduleCodeTriple  = hasRecordCode 2 <=< fromEntry

-- | MODULE_CODE_DATALAYOUT
moduleCodeDatalayout :: Match Entry Record
moduleCodeDatalayout  = hasRecordCode 3 <=< fromEntry

-- | MODULE_CODE_ASM
moduleCodeAsm :: Match Entry Record
moduleCodeAsm  = hasRecordCode 4 <=< fromEntry

moduleCodeSectionname :: Match Entry Record
moduleCodeSectionname  = hasRecordCode 5 <=< fromEntry

moduleCodeDeplib :: Match Entry Record
moduleCodeDeplib  = hasRecordCode 6 <=< fromEntry

-- | MODULE_CODE_GLOBALVAR
moduleCodeGlobalvar :: Match Entry Record
moduleCodeGlobalvar  = hasRecordCode 7 <=< fromEntry

moduleCodeFunction :: Match Entry Record
moduleCodeFunction  = hasRecordCode 8 <=< fromEntry

-- | MODULE_CODE_ALIAS
moduleCodeAlias :: Match Entry Record
moduleCodeAlias  = hasRecordCode 9 <=< fromEntry

moduleCodePurgevals :: Match Entry Record
moduleCodePurgevals  = hasRecordCode 10 <=< fromEntry

moduleCodeGcname :: Match Entry Record
moduleCodeGcname  = hasRecordCode 11 <=< fromEntry

moduleCodeComdat :: Match Entry Record
moduleCodeComdat = hasRecordCode 12 <=< fromEntry

moduleCodeVSTOffset :: Match Entry Record
moduleCodeVSTOffset = hasRecordCode 13 <=< fromEntry

moduleCodeAliasNew :: Match Entry Record
moduleCodeAliasNew = hasRecordCode 14 <=< fromEntry

moduleCodeMDValsUnused :: Match Entry Record
moduleCodeMDValsUnused = hasRecordCode 15 <=< fromEntry

moduleCodeSourceFilename :: Match Entry Record
moduleCodeSourceFilename = hasRecordCode 16 <=< fromEntry

moduleCodeHash :: Match Entry Record
moduleCodeHash = hasRecordCode 17 <=< fromEntry

moduleCodeIFunc :: Match Entry Record
moduleCodeIFunc = hasRecordCode 18 <=< fromEntry

strtabBlobId :: Match Entry Record
strtabBlobId = hasRecordCode 1 <=< fromEntry

symtabBlobId :: Match Entry Record
symtabBlobId = hasRecordCode 1 <=< fromEntry
