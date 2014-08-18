module Data.LLVM.BitCode.GetBits (
    GetBits
  , runGetBits
  , fixed, align32bits
  , label
  , isolate
  , try
  , skip
  ) where

import Data.LLVM.BitCode.BitString

import Control.Applicative (Applicative(..),Alternative(..),(<$>))
import Control.Arrow (first)
import Control.Monad (MonadPlus(..))
import Data.Bits (shiftR)
import Data.Monoid (mempty,mappend)
import Data.Word (Word32)
import qualified Data.Serialize as C


-- Bit-level Parsing -----------------------------------------------------------

newtype GetBits a = GetBits { unGetBits :: SubWord -> C.Get (a,SubWord) }

-- | Run a @GetBits@ action, returning its value, and the number of bits offset
-- into the next byte of the stream.
runGetBits :: GetBits a -> C.Get a
runGetBits m = fst `fmap` unGetBits m Aligned

instance Functor GetBits where
  {-# INLINE fmap #-}
  fmap f m = GetBits (\ off -> first f <$> unGetBits m off)

instance Applicative GetBits where
  {-# INLINE pure #-}
  pure x = GetBits (\ off -> return (x,off))

  {-# INLINE (<*>) #-}
  f <*> x = GetBits $ \ off0 -> do
    (g,off1) <- unGetBits f off0
    (y,off2) <- unGetBits x off1
    return (g y,off2)

instance Monad GetBits where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  m >>= f = GetBits $ \ off0 -> do
    (x,off1) <- unGetBits m off0
    unGetBits (f x) off1

  {-# INLINE fail #-}
  fail str = GetBits (\ _ -> fail str)

instance Alternative GetBits where
  {-# INLINE empty #-}
  empty   = GetBits (\_ -> mzero)

  {-# INLINE (<|>) #-}
  a <|> b = GetBits (\ off -> unGetBits a off <|> unGetBits b off)

instance MonadPlus GetBits where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = (<|>)


-- Sub-word Bit Parsing State --------------------------------------------------

-- This assumes that we'll pad any incoming bitcode to a 32-bit boundary, but
-- given the use of 32-bit padding already, it seems likely that all bitcode
-- files get padded to a 32-bit boundary.
data SubWord
  = SubWord !Int !Word32
  | Aligned
    deriving (Show)

splitWord :: Int -> Int -> Word32 -> (BitString,Either Int SubWord)
splitWord n l w = case compare n l of
  LT -> (toBitString n (fromIntegral w), Right (SubWord (l - n) (w `shiftR` n)))
  EQ -> (toBitString n (fromIntegral w), Right Aligned)
  GT -> (toBitString l (fromIntegral w), Left (n - l))

-- | Get a @BitString@, yielding a new partial word.
getBitString :: Int -> C.Get (BitString,SubWord)
getBitString 0 = return (mempty,Aligned)
getBitString n = getBitStringPartial n 32 =<< C.getWord32le

-- | A combination of @splitWord@ and @getBitString@ that takes an initial
-- partial word as input.
getBitStringPartial :: Int -> Int -> Word32 -> C.Get (BitString,SubWord)
getBitStringPartial n l w = case splitWord n l w of
  (bs,Right off) -> return (bs, off)
  (bs,Left n')   -> do
    (rest,off) <- getBitString n'
    return (bs `mappend` rest, off)


-- Basic Interface -------------------------------------------------------------

-- | Read zeros up to an alignment of 32-bits.
align32bits :: GetBits ()
align32bits  = GetBits $ \ off -> case off of
  Aligned     -> return ((),Aligned)
  SubWord _ 0 -> return ((),Aligned)
  SubWord _ _ -> fail "alignment padding was not zeros"

-- | Read out n bits as a @BitString@.
fixed :: Int -> GetBits BitString
fixed n = GetBits $ \ off -> case off of
  Aligned     -> getBitString n
  SubWord l w -> getBitStringPartial n l w

-- | Add a label to the error tag stack.
label :: String -> GetBits a -> GetBits a
label l m = GetBits (\ off -> C.label l (unGetBits m off))

-- | Isolate input length, in 32-bit words.
isolate :: Int -> GetBits a -> GetBits a
isolate ws m = GetBits (\ off -> C.isolate (ws * 4) (unGetBits m off))

-- | Try to parse something, returning Nothing when it fails.
--
-- XXX this will hang on to the parsing state at the Get and GetBits levels, so
-- a long-running computation will keep the current state until it finishes.
try :: GetBits a -> GetBits (Maybe a)
try m = (Just <$> m) `mplus` return Nothing

skip :: Int -> GetBits ()
skip n = do
  _ <- fixed n
  return ()
