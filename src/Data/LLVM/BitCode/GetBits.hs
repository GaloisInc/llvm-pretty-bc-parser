{-# LANGUAGE CPP #-}

module Data.LLVM.BitCode.GetBits (
    GetBits
  , runGetBits
  , fixed, align32bits
  , bytestring
  , label
  , isolate
  , try
  , skip
  ) where

import Data.LLVM.BitCode.BitString

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad (MonadPlus(..),when,replicateM_)
import qualified Data.Binary.Get as BG
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.Word (Word32)

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail( MonadFail )
import qualified Control.Monad.Fail
#endif

-- Bit-level Parsing -----------------------------------------------------------

newtype GetBits a = GetBits { unGetBits :: SubWord -> BG.Get (a,SubWord) }

-- | Run a @GetBits@ action, returning its value, and the number of bits offset
-- into the next byte of the stream.
runGetBits :: GetBits a -> BG.Get a
runGetBits m = fst `fmap` unGetBits m aligned

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

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail str = GetBits (\ _ -> fail str)
#endif

instance MonadFail GetBits where
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
  = SubWord !NumBits !Word32
    deriving (Show)

aligned :: SubWord
aligned = SubWord (Bits' 0) 0

splitWord :: NumBits -> SubWord -> (BitString, Either NumBits SubWord)
splitWord n (SubWord l w)
  | n <= l = ( toBitString n (fromIntegral w)
             , Right (SubWord (subtractBitCounts l n) (w `shiftR` bitCount n))
             )
  | otherwise = (toBitString l (fromIntegral w), Left (subtractBitCounts n l))

-- | @getBitString n@ grabs a @BitString@ of length @n@ from the next incoming
-- word, yielding the remainder partial word.  On @n@ = @0@, it does not
-- actually consume the next word.  Should not be called to read more than one
-- 32-bit word at a time (will fail on @n@ > 32).
getBitString :: NumBits -> BG.Get (BitString, SubWord)
getBitString (Bits' 0) = return (emptyBitString, aligned)
getBitString n | n > Bits' 32 =
  fail $ "getBitString: refusing to read " ++ show n ++ " (> 32) bits."
getBitString n = getBitStringPartial n . SubWord (Bits' 32) =<< BG.getWord32le

-- | @getBitStringPartial n sw@ returns a @BitString@ of length @n@ from either
-- the current subword @sw@ (with some @l@ bits available), potentially also
-- reading the next incoming word if @n@ > @l@.  Should not be called to read
-- more than one 32-bit word at a time (will fail on @n@ > 32).
getBitStringPartial :: NumBits -> SubWord -> BG.Get (BitString, SubWord)
getBitStringPartial n _ | n > Bits' 32 =
  fail $ "getBitStringPartial: refusing to read " ++ show n ++ " (> 32) bits."
getBitStringPartial n sw = case splitWord n sw of
  (bs, Right off) -> return (bs, off)
  (bs, Left n') -> do
    (rest, off) <- getBitString n'
    return (bs `joinBitString` rest, off)

-- | Skip a byte of input, which must be zero.
skipZeroByte :: BG.Get ()
skipZeroByte = do
  x <- BG.getWord8
  when (x /= 0) $ fail "alignment padding was not zeros"

-- | Get a @ByteString@ of @n@ bytes, and then align to 32 bits.
getByteString :: NumBytes -> BG.Get (ByteString, SubWord)
getByteString (Bytes' n) = do
  bs <- BG.getByteString n
  replicateM_ ((- n) `mod` 4) skipZeroByte
  return (bs, aligned)


-- Basic Interface -------------------------------------------------------------

-- | Read zeros up to an alignment of 32-bits.
align32bits :: GetBits ()
align32bits  = GetBits $ \ off -> case off of
  SubWord _ 0 -> return ((), aligned)
  SubWord _ _ -> fail "alignment padding was not zeros"

-- | Read out n bits as a @BitString@.
fixed :: NumBits -> GetBits BitString
fixed n = GetBits $ getBitStringPartial n

-- | Read out n bytes as a @ByteString@, aligning to a 32-bit boundary before and after.
bytestring :: NumBytes -> GetBits ByteString
bytestring n = GetBits $ \ off -> case off of
  SubWord _ 0 -> getByteString n
  SubWord _ _ -> fail "alignment padding was not zeros"

-- | Add a label to the error tag stack.
label :: String -> GetBits a -> GetBits a
label l m = GetBits (BG.label l . unGetBits m)

-- | Isolate input to a sub-span of the specified byte length.
isolate :: NumBytes -> GetBits a -> GetBits a
isolate (Bytes' ws) m = GetBits (BG.isolate ws . unGetBits m)

-- | Try to parse something, returning Nothing when it fails.
--
-- XXX this will hang on to the parsing state at the Get and GetBits levels, so
-- a long-running computation will keep the current state until it finishes.
try :: GetBits a -> GetBits (Maybe a)
try m = (Just <$> m) `mplus` return Nothing

skip :: NumBits -> GetBits ()
skip n = do
  _ <- fixed n
  return ()
