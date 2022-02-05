module Data.LLVM.BitCode.BitString (
    BitString(..)
  , toBitString
  , showBitString
  , fromBitString
  , maskBits
  , take, drop, splitAt
  ) where

import Data.Bits ((.&.),(.|.),shiftL,shiftR,bit,bitSizeMaybe, Bits)
import Data.Semigroup
import Numeric (showIntAtBase)

import Prelude hiding (take,drop,splitAt)

data BitString = BitString
  { bsLength :: !Int
  , bsData   :: !Integer
  } deriving Show

instance Eq BitString where
  BitString n i == BitString m j = n == m && i == j

instance Semigroup BitString where
  BitString n i <> BitString m j =
    BitString (n+m) (i .|. (j `shiftL` n))

instance Monoid BitString where
  mempty = BitString 0 0
  mappend = (<>)

-- | Given a number of bits to take, and an @Integer@, create a @BitString@.
toBitString :: Int -> Integer -> BitString
toBitString len val = BitString len (val .&. maskBits len)

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
  padding = replicate (bsLength bs - length bin) '0'
  fmt 0   = '0'
  fmt 1   = '1'
  fmt _   = error "invalid binary digit value"


-- | Generate a mask from a number of bits desired.
maskBits :: Int -> Integer
maskBits len
  | len <= 0  = 0
  | otherwise = pred (bit len)

take :: Int -> BitString -> BitString
take n bs@(BitString l i)
  | n >= l    = bs
  | otherwise = toBitString n i

drop :: Int -> BitString -> BitString
drop n (BitString l i)
  | n >= l    = mempty
  | otherwise = BitString (l - n) (i `shiftR` n)

splitAt :: Int -> BitString -> (BitString,BitString)
splitAt n bs@(BitString l i)
  | n <= 0    = (mempty, bs)
  | n >= l    = (bs, mempty)
  | otherwise = (toBitString n i, toBitString (l - n) (i `shiftR` n))
