{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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

import           Data.LLVM.BitCode.BitString

import           Control.Applicative ( Alternative(..) )
import           Control.Monad ( MonadPlus(..) )
import           Data.Bits ( shiftR, shiftL, (.&.), (.|.) )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import           GHC.Exts
import           GHC.Word

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail ( MonadFail )
import qualified Control.Monad.Fail
#endif

-- Bit-level Parsing -----------------------------------------------------------

newtype GetBits a =
  GetBits { unGetBits :: BitPosition -> BS.ByteString
                      -> (# BitsGetter a, BitPosition #)
          }

type BitPosition = (# Int#, Int# #)  -- (# current bit pos, maximum bit pos #)

type BitsGetter a = Either String a -- Left is fail


-- | Run a @GetBits@ action, returning its value, and the number of bits offset
-- into the next byte of the stream.
runGetBits :: GetBits a -> ByteString -> Either String a
runGetBits m bs =
  let !startPos# = (# 0#, bitCount# $ bytesToBits $ Bytes' $ BS.length bs #)
      !(# g, _ #) = unGetBits m startPos# bs
  in g


instance Functor GetBits where
  {-# INLINE fmap #-}
  fmap f m = GetBits $
    \ !pos# inp -> let !(# b, n# #) = unGetBits m pos# inp
                   in (# f <$> b, n# #)

instance Applicative GetBits where
  {-# INLINE pure #-}
  pure x = GetBits $ \ !pos# _ -> (# pure x, pos# #)

  {-# INLINE (<*>) #-}
  f <*> x =
    GetBits $ \ !pos# inp ->
                let !(# g, n# #) = unGetBits f pos# inp
                in case g of
                     Right g' ->
                       let !(# y, m# #) = unGetBits x n# inp
                       in case y of
                            Right y' -> (# Right $ g' y', m# #)
                            Left e -> (# Left e, m# #)
                     Left e -> (# Left e, n# #)

instance Monad GetBits where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  m >>= f = GetBits $ \ !pos# inp ->
                        let !(# g, n# #) = unGetBits m pos# inp
                            !(# gr, nr# #) = case g of
                                               Left e -> (# Left e, n# #)
                                               Right a -> unGetBits (f a) n# inp
                        in (# gr, nr# #)

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail e = GetBits $ \ p _ -> (# Left e, p #)
#endif

instance MonadFail GetBits where
  {-# INLINE fail #-}
  fail e = GetBits $ \ p _ -> (# Left e, p #)

instance Alternative GetBits where
  {-# INLINE empty #-}
  empty = GetBits $ \ p _ -> (# Left "GetBits is empty!", p #)

  {-# INLINE (<|>) #-}
  a <|> b = GetBits
            $ \ !pos# inp ->
                let !r@(# g, _ #) = unGetBits a pos# inp
                in case g of
                     Right _ -> r
                     Left _ -> unGetBits b pos# inp

instance MonadPlus GetBits where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = (<|>)


-- | Extracts an Integer value of the specified number of bits from a ByteString,
-- starting at the indicated bit position (fails with Left if the range to
-- extract is not valid... i.e. > bitLimit).  Returns the Integer value along
-- with the bit position following the extraction.

-- There are two implementations: one builds an integer value by shifting bits,
-- then shifts and masks the result to get the final value.  The other uses
-- unlifted values and avoids the final shift by being smarter about individual
-- compositions.  Their functionality should be identical, but it may be easier
-- to debug the first.

extractFromByteString' :: NumBits {-^ the last bit accessible in the ByteString -}
                       -> NumBits {-^ the bit to start extraction at -}
                       -> NumBits {-^ the number of bits to extract -}
                       -> ByteString {-^ the ByteString to extract from -}
                       -> Either String (Int, NumBits)
extractFromByteString' bitLimit startBit numBits bs =
  let Bytes' s8 = fst (bitsToBytes startBit)
      Bytes' r8 = fst (bitsToBytes numBits)
      rcnt = r8 + 2 -- 2 == pre-shift overflow byte on either side

      -- Extract the relevant bits from the ByteCode, with padding to byte
      -- boundaries into ws.
      ws = BS.take rcnt $ BS.drop s8 bs

      -- Combine the extracted bytes into an Integer value in wi.
      wi = BS.foldr (\w a -> a `shiftL` 8 .|. fromIntegral w) (0::Int) ws

      -- Mask is 0-bit based set of bits wanted in the result
      mask = ((1::Int) `shiftL` bitCount numBits) - 1

      -- Shift the desired value down to byte alignment and then discard any
      -- excess high bits.
      vi = wi `shiftR` (bitCount startBit .&. 7) .&. mask

      updPos = addBitCounts startBit numBits
  in if updPos > bitLimit
     then Left ("Attempt to read bits past limit (newPos="
                <> show updPos <> ", limit=" <> show bitLimit <> ")"
               )
     else Right (vi, updPos)

extractFromByteString :: Int# {-^ the last bit accessible in the ByteString -}
                      -> Int# {-^ the bit to start extraction at -}
                      -> Int# {-^ the number of bits to extract -}
                      -> ByteString {-^ the ByteString to extract from -}
                      -> Either String (() -> (# Int#, Int# #))
extractFromByteString !bitLim# !sBit# !nbits# bs =
     if isTrue# ((1# `uncheckedIShiftL#` (nbits#)) /=# 0#)
        -- (nbits# -# 1#) above would allow 64-bit value extraction, but this
        -- function cannot actually support a size of 64, because Int# is signed,
        -- so it doesn't properly use the high bit in numeric operations.  This
        -- seems to be OK at this point because LLVM bitcode does not attempt to
        -- encode actual 64-bit values.
     then
       let !updPos# = sBit# +# nbits#
       in if isTrue# (updPos# <=# bitLim#)
          then
            let !s8# = sBit# `uncheckedIShiftRL#` 3#
                !hop# = sBit# `andI#` 7#
                !r8# = ((hop# +# nbits# +# 7#) `uncheckedIShiftRL#` 3#)
                !mask# = (1# `uncheckedIShiftL#` nbits#) -# 1#
                -- Here, s8# is the size in 8-bit bytes, hop# is the number of
                -- bits shifted from the byte boundary, r8# is the rounded number
                -- of bytes actually needed to retrieve to get the value to
                -- account for shifting, and mask# is the mask for the final
                -- target set of bits after shifting.
#if MIN_VERSION_base(4,16,0)
                word8ToInt !w8# = word2Int# (word8ToWord# w8#)
#else
                -- technically #if !MIN_VERSION_ghc_prim(0,8,0), for GHC 9.2, but
                -- since ghc_prim isn't a direct dependency and is re-exported
                -- from base, this define needs to reference the base version.
                word8ToInt = word2Int#
#endif
                -- getB# gets a value from a byte starting at bit0 of the byte
                getB# :: Int# -> Int#
                getB# !i# =
                  case i# of
                    0# -> let !(W8# w#) = bs `BS.index` (I# s8#)
                          in word8ToInt w#
                    _ -> let !(W8# w#) = (bs `BS.index` (I# (s8# +# i#)))
                         in (word8ToInt w#) `uncheckedIShiftL#` (8# *# i#)
                -- getSB# gets a value from a byte shifting from a non-zero start
                -- bit within the byte.
                getSB# :: Int# -> Int#
                getSB# !i# =
                  case i# of
                    0# -> let !(W8# w#) = bs `BS.index` (I# s8#)
                          in (word8ToInt w#) `uncheckedIShiftRL#` hop#
                    _  -> let !(W8# w#) = bs `BS.index` (I# (s8# +# i#))
                              !shft# = (8# *# i#) -# hop#
                          in (word8ToInt w#) `uncheckedIShiftL#` shft#
                !vi# = mask# `andI#`
                       (case hop# of
                          0# -> case r8# of
                                  1# -> getB# 0#
                                  2# -> getB# 0# `orI#` getB# 1#
                                  3# -> getB# 0# `orI#` getB# 1# `orI#`
                                        getB# 2#
                                  4# -> getB# 0# `orI#` getB# 1# `orI#`
                                        getB# 2# `orI#` getB# 3#
                                  5# -> getB# 0# `orI#` getB# 1# `orI#`
                                        getB# 2# `orI#` getB# 3# `orI#`
                                        getB# 4#
                                  6# -> getB# 0# `orI#` getB# 1# `orI#`
                                        getB# 2# `orI#` getB# 3# `orI#`
                                        getB# 4# `orI#` getB# 5#
                                  7# -> getB# 0# `orI#` getB# 1# `orI#`
                                        getB# 2# `orI#` getB# 3# `orI#`
                                        getB# 4# `orI#` getB# 5# `orI#`
                                        getB# 6#
                                  8# -> getB# 0# `orI#` getB# 1# `orI#`
                                        getB# 2# `orI#` getB# 3# `orI#`
                                        getB# 4# `orI#` getB# 5# `orI#`
                                        getB# 6# `orI#` getB# 7#
                                  -- This is the catch-all loop for other sizes
                                  -- not addressed above.
                                  _ -> let join !(W8# w#) !(I# a#) =
                                             I# ((a# `uncheckedIShiftL#` 8#)
                                                 `orI#` (word8ToInt w#))
                                           bs' = BS.take (I# (r8# +# 2#))
                                                 $ BS.drop (I# s8#) bs
                                           !(I# v#) = BS.foldr join (0::Int) bs'
                                       in mask# `andI#` (v# `uncheckedIShiftRL#` hop#)
                          _ -> case r8# of
                                 1# -> getSB# 0#
                                 2# -> getSB# 0# `orI#` getSB# 1#
                                 3# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2#
                                 4# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2# `orI#` getSB# 3#
                                 5# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2# `orI#` getSB# 3# `orI#`
                                       getSB# 4#
                                 6# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2# `orI#` getSB# 3# `orI#`
                                       getSB# 4# `orI#` getSB# 5#
                                 7# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2# `orI#` getSB# 3# `orI#`
                                       getSB# 4# `orI#` getSB# 5# `orI#`
                                       getSB# 6#
                                 8# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2# `orI#` getSB# 3# `orI#`
                                       getSB# 4# `orI#` getSB# 5# `orI#`
                                       getSB# 6# `orI#` getSB# 7#
                                 -- n.b. these are hand-unrolled cases for common
                                 -- sizes this is called for.
                                 9# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                       getSB# 2# `orI#` getSB# 3# `orI#`
                                       getSB# 4# `orI#` getSB# 5# `orI#`
                                       getSB# 6# `orI#` getSB# 7# `orI#`
                                       getSB# 8#
                                 18# -> getSB# 0# `orI#` getSB# 1# `orI#`
                                        getSB# 2# `orI#` getSB# 3# `orI#`
                                        getSB# 4# `orI#` getSB# 5# `orI#`
                                        getSB# 6# `orI#` getSB# 7# `orI#`
                                        getSB# 8# `orI#` getSB# 9# `orI#`
                                        getSB# 10# `orI#` getSB# 11# `orI#`
                                        getSB# 12# `orI#` getSB# 13# `orI#`
                                        getSB# 14# `orI#` getSB# 15# `orI#`
                                        getSB# 16# `orI#` getSB# 17#
                                 -- This is the catch-all loop for other sizes
                                 -- not addressed above.
                                 _ -> let join !(W8# w#) !(I# a#) =
                                            I# ((a# `uncheckedIShiftL#` 8#)
                                                `orI#` (word8ToInt w#))
                                          bs' = BS.take (I# (r8# +# 2#))
                                                $ BS.drop (I# s8#) bs
                                          !(I# v#) = BS.foldr join (0::Int) bs'
                                      in mask# `andI#` (v# `uncheckedIShiftRL#` hop#)
                       )
            in Right $ \_ -> (# vi#, updPos# #)
          else Left "Attempt to read bits past limit"
     else
       -- BitString stores an Int, but number of extracted bits is larger than
       -- an Int can represent.
       Left "Attempt to extracted large value"


-- Basic Interface -------------------------------------------------------------

-- | Read zeros up to an alignment of 32-bits.
align32bits :: GetBits ()
align32bits  = GetBits $ \ !pos# inp ->
  let !(# curBit#, ttlBits# #) = pos#
      !s32# = curBit# `andI#` 31#
      !r32# = 32# -# s32#  -- num bits to reach next 32-bit boundary
      nonZero = "alignments @" <> show (I# curBit#)
                <> " not zeroes up to 32-bit boundary"
  in if isTrue# (s32# ==# 0#)
     then (# Right (), pos# #)
     else case extractFromByteString ttlBits# curBit# r32# inp of
            Right getRes ->
              let !(# vi#, newPos# #) = getRes ()
              in if isTrue# (vi# ==# 0#)
                 then (# Right (), (# newPos#, ttlBits# #) #)
                 else (# Left nonZero, pos# #)
            Left e -> (# Left e, pos# #)


-- | Read out n bits as a @BitString@.
fixed :: NumBits -> GetBits BitString
fixed !(Bits' (I# n#)) = GetBits
  $ \ !s@(# cur#, lim# #) ->
      \inp ->
        case extractFromByteString lim# cur# n# inp of
          Right getRes ->
            let !(# v#, p# #) = getRes ()
            in (# pure $ toBitString (Bits' (I# n#)) (I# v#)
               , (# p#, lim# #)
               #)
          Left e -> (# Left e, s #)


-- | Read out n bytes as a @ByteString@, aligning to a 32-bit boundary before and
-- after.
bytestring :: NumBytes -> GetBits ByteString
bytestring n@(Bytes' nbytes) = do
  align32bits
  r <- GetBits
       $ \ !(# pos#, lim# #) ->
           \inp ->
             let !sbyte# = pos# `uncheckedIShiftRL#` 3# -- known to be aligned
                 !endAt# = pos# +# bitCount# (bytesToBits n)
                 !end# = (# endAt#, lim# #)
                 err = "Sub-bytestring attempted beyond end of input bytestring"
             in if isTrue# (endAt# <=# lim#)
                then (# pure $ BS.take nbytes $ BS.drop (I# sbyte#) inp, end# #)
                else (# Left err, end# #)
  align32bits
  return r


-- | Add a label to the error tag stack.
label :: String -> GetBits a -> GetBits a
label l m = GetBits $ \ !pos# inp ->
                        let !(# j, n# #) = unGetBits m pos# inp
                        in case j of
                             Left e -> (# Left $ e <> "\n  " <> l, n# #)
                             Right r -> (# Right r, n# #)


-- | Isolate input to a sub-span of the specified byte length.
isolate :: NumBytes -> GetBits a -> GetBits a
isolate ws m =
  GetBits $ \ !(# pos#, lim# #) ->
              \inp ->
                let !l# = pos# +# bitCount# (bytesToBits ws)
                    !(# r, (# x#, _ #) #) = unGetBits m (# pos#, l# #) inp
                in (# r, (# x#, lim# #) #)


-- | Try to parse something, returning Nothing when it fails.

try :: GetBits a -> GetBits (Maybe a)
try m = (Just <$> m) `mplus` return Nothing


-- | Skips the specified number of bits

skip :: NumBits -> GetBits ()
skip !(Bits' (I# n#)) =
  GetBits $ \ !(# cur#, lim# #) ->
              let !newLoc# = cur# +# n#
                  !newPos# = (# newLoc#, lim# #)
              in if isTrue# (newLoc# ># lim#)
                 then \_ -> (# Left "skipped past end of bytestring"
                            , newPos#
                              #)
                 else \_ -> (# Right (), newPos# #)
