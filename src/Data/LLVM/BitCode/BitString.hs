{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.LLVM.BitCode.BitString
  (
    BitString
  , emptyBitString
  , toBitString
  , showBitString
  , fromBitString
  , bitStringValue
  , take, drop
  , joinBitString
  , NumBits, NumBytes, pattern Bits', pattern Bytes'
  , bitCount, bitCount#
  , bitsToBytes, bytesToBits
  , bitMask
  , addBitCounts
  , subtractBitCounts
  )
where

#ifdef QUICK
import Data.Bits ( Bits )
import Numeric ( showIntAtBase )
#else
import Data.Bits ( bit, bitSizeMaybe, Bits )
import Numeric ( showIntAtBase, showHex )
#endif
import GHC.Exts

import Prelude hiding (take,drop,splitAt)


----------------------------------------------------------------------
-- Define some convenience newtypes to clarify whether the count of bits or count
-- of bytes is being referenced, and to convert between the two.

newtype NumBits = NumBits Int deriving (Show, Eq, Ord)
newtype NumBytes = NumBytes Int deriving (Show, Eq, Ord)

pattern Bits' :: Int -> NumBits
pattern Bits' n = NumBits n
{-# COMPLETE Bits' #-}

pattern Bytes' :: Int -> NumBytes
pattern Bytes' n = NumBytes n
{-# COMPLETE Bytes' #-}

bitCount :: NumBits -> Int
bitCount (NumBits n) = n

bitCount# :: NumBits -> Int#
bitCount# (NumBits (I# n#)) = n#

{-# INLINE addBitCounts #-}
addBitCounts :: NumBits -> NumBits -> NumBits
addBitCounts (NumBits (I# a#)) (NumBits (I# b#)) = NumBits (I# (a# +# b#))

{-# INLINE subtractBitCounts #-}
subtractBitCounts :: NumBits -> NumBits -> NumBits
subtractBitCounts (NumBits (I# a#)) (NumBits (I# b#)) = NumBits (I# (a# -# b#))

{-# INLINE bytesToBits #-}
bitsToBytes :: NumBits -> (NumBytes, NumBits)
bitsToBytes (NumBits (I# n#)) = ( NumBytes (I# (n# `uncheckedIShiftRL#` 3#))
                                , NumBits (I# (n# `andI#` 7#))
                                )

{-# INLINE bitsToBytes #-}
bytesToBits :: NumBytes -> NumBits
bytesToBits (NumBytes (I# n#)) = NumBits (I# (n# `uncheckedIShiftL#` 3#))

----------------------------------------------------------------------

data BitString = BitString
  { bsLength :: !NumBits
  , bsData   :: !Word
    -- Note: the bsData was originally an Integer, which allows an essentially
    -- unlimited size value.  However, this adds some overhead to various
    -- computations, and since LLVM Bitcode is unlikely to ever represent values
    -- greater than the native size (64 bits) as discrete values.  By changing
    -- this to @Word@ (which is verified to be 64 bits), the use of unboxed
    -- calculations is enabled for better performance.
    --
    -- Note that Word is used instead of Word64; in GHC pre 9.x, Word64 was
    -- intended to represent a 64-bit value on a 32-bit system.
  } deriving (Show, Eq)

-- Verify a Word is 64-bits (at compile time)
$(return $ if isTrue# ((int2Word# 3#) `eqWord#`
                       (((int2Word# 0xF0#) `uncheckedShiftL#` 58#)
                        `uncheckedShiftRL#` 62#))
           then []
           else error "Word type must be 64-bits!"
 )


-- | Create an empty BitString

emptyBitString :: BitString
emptyBitString = BitString (NumBits 0) 0


-- | Join two BitString representations together to form a single larger
-- BitString.  The first BitString is the \"lower\" value portion of the resulting
-- BitString.

joinBitString :: BitString -> BitString -> BitString
joinBitString (BitString (Bits' (I# szA#)) (W# a#))
              (BitString (Bits' (I# szB#)) (W# b#)) =
  BitString { bsLength = NumBits (I# (szA# +# szB#))
            , bsData = W# (a# `or#` (b# `uncheckedShiftL#` szA#))
            }

bitMask :: NumBits -> Word#
bitMask (Bits' (I# len#)) =
  ((int2Word# 1#) `uncheckedShiftL#` len#) `minusWord#` (int2Word# 1#)


-- | Given a number of bits to take, and an @Integer@, create a @BitString@.

toBitString :: NumBits -> Word -> BitString
toBitString len (W# val#) = BitString len (W# (val# `and#` (bitMask len)))


-- | Extract the referenced Integer value from a BitString

bitStringValue :: BitString -> Word
bitStringValue = bsData


-- | Extract a target (Num) value of the desired type from a BitString (using
-- fromInteger to perform the target type conversion).

fromBitString :: (Num a, Bits a) => BitString -> a
#ifdef QUICK
fromBitString (BitString _ i) = x
#else
fromBitString (BitString l i) =
  case bitSizeMaybe x of
    Nothing -> x
    Just n
      -- Verify that the bitstring size is less than the target size, or if it is
      -- greater, that the extra upper bits are all zero.
      | n >= bitCount l || (ival < bit n) -> x
      | otherwise -> error (unwords
           [ "Data.LLVM.BitCode.BitString.fromBitString: bitstring value of length", show l
           , "(mask=0x" <> showHex i ")"
           , "could not be parsed into type with only", show n, "bits"
           ])
#endif
 where
 x    = fromInteger ival  -- use Num to convert the Integer to the target type
 ival = toInteger i  -- convert input to an Integer for ^^


showBitString :: BitString -> ShowS
showBitString bs = showString padding . showString bin
  where
  bin     = showIntAtBase 2 fmt (bsData bs) ""
  padding = replicate (bitCount (bsLength bs) - length bin) '0'
  fmt 0   = '0'
  fmt 1   = '1'
  fmt _   = error "invalid binary digit value"


-- | Extract a smaller BitString with the specified number of bits from the
-- \"start\" of a larger BitString.
take :: NumBits -> BitString -> BitString
take n bs@(BitString l i)
  | n >= l    = bs
  | otherwise = toBitString n i


-- | Remove the specified number of bits from the beginning of a BitString and
-- return the remaining as a smaller BitString.

drop :: NumBits -> BitString -> BitString
drop !n !(BitString l v)
  | n >= l    = emptyBitString
  | otherwise =
      let !(I# n#) = bitCount n
          !(I# l#) = bitCount l
          !(W# v#) = v
      in BitString (NumBits (I# (l# -# n#))) (W# (v# `uncheckedShiftRL#` n#))
