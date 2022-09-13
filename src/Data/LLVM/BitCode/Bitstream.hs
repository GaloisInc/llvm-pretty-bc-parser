{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}

module Data.LLVM.BitCode.Bitstream (
    Bitstream(..)
  , Entry(..)
  , AbbrevIdWidth, AbbrevId(..)
  , Block(..), BlockId
  , RecordId
  , UnabbrevRecord(..)
  , DefineAbbrev(..), AbbrevOp(..)
  , AbbrevRecord(..), Field(..)

  , getBitstream, parseBitstream
  , getBitCodeBitstream, parseBitCodeBitstream, parseBitCodeBitstreamLazy
  , parseMetadataStringLengths
  ) where

import           Control.Monad ( unless, replicateM, guard )
import           Data.Bits ( Bits )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.LLVM.BitCode.BitString as BS
import           Data.LLVM.BitCode.GetBits
import qualified Data.Map as Map
#ifdef QUICK
import           GHC.Exts
#endif
import           GHC.Word


-- Primitive Reads -------------------------------------------------------------

-- | Parse a @Bool@ out of a single bit.
boolean :: GetBits Bool
boolean = do i <- fixedInt (Bits' 1)
             return $ 1 == i


-- | Parse a Num type out of n-bits.
numeric :: (Num a, Bits a) => NumBits -> GetBits a
#ifdef QUICK
numeric n = fromInteger . toInteger <$> fixedInt n
#else
numeric n = fromBitString <$> fixed n
#endif


-- | Get a @BitString@ formatted as vbr.
vbr :: NumBits -> GetBits BitString
vbr n = loop emptyBitString
  where
  len      = subtractBitCounts n (Bits' 1)
  loop acc = acc `seq` do
    chunk <- fixed len
    cont  <- boolean
    let acc' = acc `joinBitString` chunk
    if cont
       then loop acc'
       else return acc'

#ifdef QUICK
vbrInt :: NumBits -> GetBits Int
vbrInt n@(Bits' (I# n#)) =
  let !cmask# = 1# `uncheckedIShiftL#` (n# -# 1#)
      loop = do ic <- fixedInt n
                let !(I# ic#) = ic
                if isTrue# ((ic# `andI#` cmask#) ==# 0#)
                  then return ic
                  else do nxt <- loop
                          let !(I# nxt#) = nxt
                          let nxtshft# = nxt# `uncheckedIShiftL#` (n# -# 1#)
                          return (I# ((ic# `xorI#` cmask#)`orI#` nxtshft#))
  in loop
#endif


-- | Process a variable-bit encoded integer.
vbrNum :: (Num a, Bits a) => NumBits -> GetBits a
#ifdef QUICK
vbrNum = fmap (fromInteger . toInteger) . vbrInt
#else
vbrNum n = fromBitString <$> vbr n
#endif

-- | Decode a 6-bit encoded character.
char6 :: GetBits Word8
char6  = do
#ifdef QUICK
  (I# i#) <- fixedInt (Bits' 6)
#if MIN_VERSION_base(4,16,0)
  let toWrd8 iv# = W8# (wordToWord8# (int2Word# iv#))
#else
  let toWrd8 iv# = W8# (int2Word# iv#)
#endif
  if isTrue# (i# <=# 25#)
  then return (toWrd8 (i# +# 97#))
  else if isTrue# (i# <=# 51#)
       then return (toWrd8 (i# +# 39#))
       else if isTrue# (i# <=# 61#)
            then return (toWrd8 (i# -# 4#))
            else if isTrue# (i# ==# 62#)
                 then return (fromIntegral (fromEnum '.'))
                 else return (fromIntegral (fromEnum '_'))
#else
  word <- numeric $ Bits' 6
  case word of
    n | 0  <= n && n <= 25 -> return (n + 97)
      | 26 <= n && n <= 51 -> return (n + 39)
      | 52 <= n && n <= 61 -> return (n - 4)
    62                     -> return (fromIntegral (fromEnum '.'))
    63                     -> return (fromIntegral (fromEnum '_'))
    _                      -> fail "invalid char6"
#endif


-- Bitstream Parsing -----------------------------------------------------------

data Bitstream = Bitstream
  { bsAppMagic :: !Word16
  , bsEntries  :: [Entry]
  } deriving (Show)

parseBitstream :: S.ByteString -> Either String Bitstream
parseBitstream = runGetBits getBitstream

parseBitCodeBitstream :: S.ByteString -> Either String Bitstream
parseBitCodeBitstream = parseBitCodeBitstreamLazy . L.fromStrict

parseBitCodeBitstreamLazy :: L.ByteString -> Either String Bitstream
parseBitCodeBitstreamLazy = runGetBits getBitCodeBitstream . L.toStrict

-- | The magic constant at the beginning of all llvm-bitcode files.
bcMagicConst :: BitString
bcMagicConst  = toBitString (Bits' 8) 0x42
                `joinBitString`
                toBitString (Bits' 8) 0x43

-- | Parse a @Bitstream@ from either a normal bitcode file, or a wrapped
-- bitcode.
getBitCodeBitstream :: GetBits Bitstream
getBitCodeBitstream  = label "llvm-bitstream" $ do
  mb <- try guardWrapperMagic
  case mb of
    Nothing -> getBitstream
    Just () -> do
      skip $ Bits' 32 -- Version
      off <- fixed $ Bits' 32
      -- the offset should always be 20 (5 word32 values)
      unless (fromBitString off == (20 :: Int))
          (fail ("invalid offset value: " ++ show off))
      size <- Bytes' . fromBitString <$> (fixed $ Bits' 32)
      skip $ Bits' 32 -- CPUType
      isolate size getBitstream

bcWrapperMagicConst :: BitString
bcWrapperMagicConst  =
  foldr1 joinBitString [ byte 0xDE, byte 0xC0, byte 0x17, byte 0x0B]
  where
  byte = toBitString (Bits' 8)

guardWrapperMagic :: GetBits ()
guardWrapperMagic  = do
  magic <- fixed (Bits' 32)
  guard (magic == bcWrapperMagicConst)

-- | Parse a @Bitstream@.
getBitstream :: GetBits Bitstream
getBitstream  = label "bitstream" $ do
  bc       <- fixed $ Bits' 16
  unless (bc == bcMagicConst) (fail "Invalid magic number")
  appMagic <- numeric $ Bits' 16
  entries  <- getTopLevelEntries
  return Bitstream
    { bsAppMagic = appMagic
    , bsEntries  = entries
    }

data Entry
  = EntryBlock          Block
  | EntryUnabbrevRecord UnabbrevRecord
  | EntryDefineAbbrev   DefineAbbrev
  | EntryAbbrevRecord   AbbrevRecord
    deriving (Show)

-- | Parse top-level entries.
getTopLevelEntries :: GetBits [Entry]
getTopLevelEntries  = fst <$> getEntries (Bits' 2) Map.empty emptyAbbrevMap True

-- | Get as many entries as we can parse.
getEntries :: AbbrevIdWidth -> BlockInfoMap -> AbbrevMap -> Bool
           -> GetBits ([Entry],BlockInfoMap)
getEntries aw bim0 am0 endBlocksFail = loop bim0 am0
  where
  loop bim am = do
    let finish = return ([],bim)
    mb <- try (getAbbrevId aw)
    case mb of
      Nothing  -> finish
      Just aid -> do
        let continue bim' am' k = do
              (rest,bim'') <- loop bim' am'
              return (k rest,bim'')
        label (show aid) $ case aid of

          END_BLOCK | endBlocksFail -> fail "unexpected END_BLOCK"
                    | otherwise     -> align32bits >> finish

          ENTER_SUBBLOCK -> do
            (block,bim') <- getBlock bim
            continue bim' am (EntryBlock block :)

          DEFINE_ABBREV -> do
            def <- getDefineAbbrev
            continue bim (insertAbbrev def am) (EntryDefineAbbrev def :)

          UNABBREV_RECORD -> do
            record <- getUnabbrevRecord
            continue bim am (EntryUnabbrevRecord record :)

          ABBREV_RECORD rid -> case lookupAbbrev rid am of
            Nothing  -> fail ("unknown abbrev id: " ++ show rid)
            Just def -> do
              record <- getAbbrevRecord aw rid def
              continue bim am (EntryAbbrevRecord record :)


-- Abbreviation IDs ------------------------------------------------------------

type AbbrevIdWidth = NumBits

data AbbrevId
  = END_BLOCK
  | ENTER_SUBBLOCK
  | DEFINE_ABBREV
  | UNABBREV_RECORD
  | ABBREV_RECORD !RecordId
    deriving (Ord,Eq,Show)

-- | Retrieve an @AbbrevId@, with the current width for the containing block.
getAbbrevId :: AbbrevIdWidth -> GetBits AbbrevId
getAbbrevId aw = label "abbreviation" $ do
  aid <- numeric aw
  case aid of
    0 -> return END_BLOCK
    1 -> return ENTER_SUBBLOCK
    2 -> return DEFINE_ABBREV
    3 -> return UNABBREV_RECORD
    _ -> return (ABBREV_RECORD aid)


-- Abbreviation Definitions ----------------------------------------------------

data DefineAbbrev = DefineAbbrev
  { defineOps    :: [AbbrevOp]
  } deriving (Show)

-- | Parse an abbreviation definition.
getDefineAbbrev :: GetBits DefineAbbrev
getDefineAbbrev  =
  label "define abbrev" (DefineAbbrev `fmap` (getAbbrevOps =<< vbrNum (Bits' 5)))

data AbbrevOp
  = OpLiteral !BitString
  | OpFixed   !NumBits
  | OpVBR     !NumBits
  | OpArray    AbbrevOp
  | OpChar6
  | OpBlob
    deriving (Show)

-- | Parse n abbreviation operands.
getAbbrevOps :: Int -> GetBits [AbbrevOp]
getAbbrevOps 0 = return []
getAbbrevOps n = do
  (op,consumed) <- getAbbrevOp
  rest          <- getAbbrevOps (n - consumed)
  return (op:rest)

-- | Parse an abbreviation operand.
getAbbrevOp :: GetBits (AbbrevOp,Int)
getAbbrevOp  = label "abbrevop" $ do
  let one = fmap (\x -> (x,1))
  isLiteral <- boolean
  if isLiteral
     then one (OpLiteral <$> (vbr $ Bits' 8))
     else do
       enc <- numeric $ Bits' 3
       case enc :: Word8 of
         1 -> one (OpFixed . Bits' <$> vbrNum (Bits' 5))
         2 -> one (OpVBR   . Bits' <$> vbrNum (Bits' 5))
         3 -> do
           (op,n) <- getAbbrevOp
           return (OpArray op, n+1)
         4 -> one (return OpChar6)
         5 -> one (return OpBlob)
         _ -> fail ("invalid encoding: " ++ show enc)


-- Abbrev Definition Maps ------------------------------------------------------

data AbbrevMap = AbbrevMap
  { amNextId  :: !RecordId
  , amDefines :: Map.Map RecordId DefineAbbrev
  } deriving (Show)

emptyAbbrevMap :: AbbrevMap
emptyAbbrevMap  = AbbrevMap
  { amNextId  = 4
  , amDefines = Map.empty
  }

insertAbbrev :: DefineAbbrev -> AbbrevMap -> AbbrevMap
insertAbbrev def m = m
  { amNextId  = amNextId m + 1
  , amDefines = Map.insert (amNextId m) def (amDefines m)
  }

lookupAbbrev :: RecordId -> AbbrevMap -> Maybe DefineAbbrev
lookupAbbrev aid = Map.lookup aid . amDefines


-- Blocks ----------------------------------------------------------------------

type BlockId = Word32

-- XXX Are 32-bit words big enough to house the encoded numbers?  Are they too
-- big, and waste space in most cases?
data Block = Block
  { blockId           :: !BlockId
  , blockNewAbbrevLen :: !AbbrevIdWidth
  , blockLength       :: !NumBytes
  , blockEntries      :: [Entry]
  } deriving (Show)

-- | Parse a block, optionally extending the known block info metadata.
getBlock :: BlockInfoMap -> GetBits (Block, BlockInfoMap)
getBlock bim = do
  (block,bim') <- getGenericBlock bim
  case blockId block of

    0 -> do
      bim'' <- processBlockInfo block bim'
      return (block,bim'')

    _ -> return (block, bim')

-- | A generic block.
getGenericBlock :: BlockInfoMap -> GetBits (Block,BlockInfoMap)
getGenericBlock bim = label "block " $ do
  blockid      <- vbrNum $ Bits' 8
  newabbrevlen <- Bits' <$> (vbrNum $ Bits' 4)
  align32bits
  -- Block length in the bitcode is the number of 32-bit longwords; internally it
  -- is stored as the number of bytes.
  blocklen     <- Bytes' . (*4) <$> (numeric $ Bits' 32)
  let am = lookupAbbrevMap blockid bim
  (entries,bim') <- isolate blocklen (getEntries newabbrevlen bim am False)
  let block = Block
        { blockId           = blockid
        , blockNewAbbrevLen = newabbrevlen
        , blockLength       = blocklen
        , blockEntries      = entries
        }
  return (block,bim')


-- Block Metadata --------------------------------------------------------------

data BlockInfo = BlockInfo
  { infoId      :: !BlockId
  , infoAbbrevs :: AbbrevMap
  } deriving (Show)

-- | Extend the set of abbreviation definitions for a @BlockInfo@.
addAbbrev :: DefineAbbrev -> BlockInfo -> BlockInfo
addAbbrev a bi = bi { infoAbbrevs = insertAbbrev a (infoAbbrevs bi) }

type BlockInfoMap = Map.Map BlockId BlockInfo

-- | Add a @BlockInfo@ to the set of known metadata.
insertBlockInfo :: BlockInfo -> BlockInfoMap -> BlockInfoMap
insertBlockInfo bi bim = Map.insert (infoId bi) bi bim

-- | Lookup the abbreviations defined for a block, falling back on an empty set
-- if there aren't any.
lookupAbbrevMap :: BlockId -> BlockInfoMap -> AbbrevMap
lookupAbbrevMap bid bim = maybe emptyAbbrevMap infoAbbrevs (Map.lookup bid bim)

-- | Process a generic block with blockid 0, adding all the metadata that it
-- defines to the BlockInfoMap provided.
processBlockInfo :: Block -> BlockInfoMap -> GetBits BlockInfoMap
processBlockInfo bl bim0 = case blockEntries bl of
  EntryUnabbrevRecord record:es
    | Just bi0 <- unabbrevSetBid record -> loop bim0 bi0 es
  _                                     -> fail "invalid BLOCKINFO block"
  where
  closeInfo bi bim   = insertBlockInfo bi bim
  loop bim bi []     = return (closeInfo bi bim)
  loop bim bi (e:es) = case e of

    EntryDefineAbbrev def -> loop bim (addAbbrev def bi) es

    EntryUnabbrevRecord record
      | Just bi' <- unabbrevSetBid record -> loop (closeInfo bi bim) bi' es

    -- XXX there are more interesting records, but we don't process them yet
    _ -> loop bim bi es


-- Unabbreviated Records -------------------------------------------------------

type RecordId = Int

data UnabbrevRecord = UnabbrevRecord
  { unabbrevCode :: !RecordId
  , unabbrevOps  :: [BitString]
  } deriving (Show)

-- | Parse an unabbreviated record.
getUnabbrevRecord :: GetBits UnabbrevRecord
getUnabbrevRecord  = label "unabbreviated record" $ do
  code   <- vbrNum $ Bits' 6
  numops <- vbrNum $ Bits' 6
  ops    <- replicateM numops (vbr $ Bits' 6)
  return UnabbrevRecord
    { unabbrevCode = code
    , unabbrevOps  = ops
    }

-- | Turn a SETBIT unabbreviated record into an empty @BlockInfo@.
unabbrevSetBid :: UnabbrevRecord -> Maybe BlockInfo
unabbrevSetBid record = do
  guard (unabbrevCode record == 1)
  guard (not (null (unabbrevOps record)))
  return BlockInfo
    { infoId      = fromBitString (unabbrevOps record !! 0)
    , infoAbbrevs = emptyAbbrevMap
    }


-- Abbreviated Records ---------------------------------------------------------

data AbbrevRecord = AbbrevRecord
  { abbrevIdWidth :: !AbbrevIdWidth
  , abbrevId      :: !RecordId
  , abbrevFields  :: [Field]
  } deriving (Show)

-- | Given its definition, parse an abbreviated record.
getAbbrevRecord :: AbbrevIdWidth -> RecordId -> DefineAbbrev
                -> GetBits AbbrevRecord
getAbbrevRecord aw aid def =
  label "abbreviated record" (AbbrevRecord aw aid <$> getFields def)

data Field
  = FieldLiteral !BitString
  | FieldFixed   !BitString
  | FieldVBR     !BitString
  | FieldArray    [Field]
  | FieldChar6   !Word8
  | FieldBlob    !S.ByteString
    deriving Show

getFields :: DefineAbbrev -> GetBits [Field]
getFields def = mapM interpAbbrevOp (defineOps def)

-- | Interpret a single abbreviation operation.
interpAbbrevOp :: AbbrevOp -> GetBits Field
interpAbbrevOp op = label (show op) $ case op of

  OpLiteral val -> return (FieldLiteral val)

  OpFixed width -> FieldFixed <$> fixed width

  OpVBR width -> FieldVBR <$> vbr width

  OpArray ty -> do
    len <- vbrNum $ Bits' 6
    FieldArray <$> replicateM len (interpAbbrevOp ty)

  OpChar6 -> FieldChar6 <$> char6

  OpBlob -> do
    len   <- Bytes' <$> (vbrNum $ Bits' 6)
    bytes <- bytestring len
    return (FieldBlob bytes)


-- Metadata String Lengths -----------------------------------------------------

parseMetadataStringLengths :: Int -> S.ByteString -> Either String [Int]
parseMetadataStringLengths n = runGetBits (replicateM n (vbrNum $ Bits' 6))
