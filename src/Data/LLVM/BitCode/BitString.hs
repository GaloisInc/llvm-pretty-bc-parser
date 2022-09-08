{-# LANGUAGE PatternSynonyms #-}

module Data.LLVM.BitCode.BitString (
    BitString(BitString)
  , toBitString
  , showBitString
  , fromBitString
  , bitStringValue
  , maskBits
  , take, drop
  , NumBits, NumBytes, pattern Bits', pattern Bytes'
  , bitCount
  , addBitCounts
  , subtractBitCounts
  ) where

import Data.Bits ((.&.),(.|.),shiftL,shiftR,bit,bitSizeMaybe, Bits)
import Data.Semigroup
import Numeric (showIntAtBase)

import Prelude hiding (take,drop,splitAt)

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

addBitCounts :: NumBits -> NumBits -> NumBits
addBitCounts (NumBits a) (NumBits b) = NumBits $ a + b

subtractBitCounts :: NumBits -> NumBits -> NumBits
subtractBitCounts (NumBits a) (NumBits b) = NumBits $ a - b

data BitString = BitString
  { bsLength :: !NumBits
  , bsData   :: !Integer
  } deriving Show

instance Eq BitString where
  BitString n i == BitString m j = n == m && i == j

instance Semigroup BitString where
  BitString n@(NumBits n') i <> BitString m j =
    BitString (addBitCounts n m) (i .|. (j `shiftL` n'))

instance Monoid BitString where
  mempty = BitString (NumBits 0) 0
  mappend = (<>)

-- | Given a number of bits to take, and an @Integer@, create a @BitString@.
toBitString :: NumBits -> Integer -> BitString
toBitString len val = BitString len (val .&. maskBits len)

bitStringValue :: BitString -> Integer
bitStringValue = bsData

fromBitString :: (Num a, Bits a) => BitString -> a
fromBitString (BitString l i) =
  case bitSizeMaybe x of
    Nothing -> x
    Just n
      | 0 <= ival && ival < bit n -> x
      | otherwise -> error (unwords
           [ "Data.LLVM.BitCode.BitString.fromBitString: bitstring value of length", show l
           , "(", show i, ")"
           , "could not be parsed into type with only", show n, "bits"
           ])
 where
 x    = fromInteger ival
 ival = i .&. maskBits l

showBitString :: BitString -> ShowS
showBitString bs = showString padding . showString bin
  where
  bin     = showIntAtBase 2 fmt (bsData bs) ""
  padding = replicate (bitCount (bsLength bs) - length bin) '0'
  fmt 0   = '0'
  fmt 1   = '1'
  fmt _   = error "invalid binary digit value"


-- | Generate a mask from a number of bits desired.
maskBits :: NumBits -> Integer
maskBits (NumBits len)
  | len <= 0  = 0
  | otherwise = pred (bit len)

take :: NumBits -> BitString -> BitString
take n bs@(BitString l i)
  | n >= l    = bs
  | otherwise = toBitString n i

drop :: NumBits -> BitString -> BitString
drop n (BitString l i)
  | n >= l    = mempty
  | otherwise = BitString
                (NumBits $ bitCount l - bitCount n)
                (i `shiftR` (bitCount n))
