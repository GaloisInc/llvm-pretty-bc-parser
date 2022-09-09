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
  GetBits { unGetBits :: Either String -- Left is outer fail
                         (BitPosition -> BS.ByteString
                           -> (# BitsGetter a, BitPosition #)) }

type BitPosition = (# Int#, Int# #)

type BitsGetter a = BS.ByteString -> Either String a -- Left is inner fail


-- | Run a @GetBits@ action, returning its value, and the number of bits offset
-- into the next byte of the stream.
runGetBits :: GetBits a -> ByteString -> Either String a
runGetBits m bs =
  let !startPos# = (# 0#, bitCount# $ bytesToBits $ Bytes' $ BS.length bs #)
  in do k <- unGetBits m
        let !(# g, _ #) = k startPos# bs
        g bs


instance Functor GetBits where
  {-# INLINE fmap #-}
  fmap f m = GetBits $ case unGetBits m of
    Left e -> Left e
    Right g -> Right $ \ !pos# inp -> let !(# b, n# #) = g pos# inp
                                      in (# \bs -> f <$> b bs, n# #)

instance Applicative GetBits where
  {-# INLINE pure #-}
  pure x = GetBits $ pure $ \ !pos# _ -> (# const $ pure x, pos# #)

  {-# INLINE (<*>) #-}
  f <*> x =
    GetBits $ do k <- unGetBits f
                 l <- unGetBits x
                 return $ \ !pos# inp -> let !(# g, n# #) = k pos# inp
                                             !(# y, m# #) = l n# inp
                                          in (# \_ -> do g' <- g inp
                                                         y' <- y inp
                                                         return $ g' y'
                                             , m#
                                             #)

instance Monad GetBits where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  m >>= f =
    case unGetBits m of
      Left e -> GetBits $ Left e
      Right k -> GetBits $ pure $ \ !pos# inp ->
        let !(# g, n# #) = k pos# inp
            !(# gr, nr# #) = case g inp of  -- KWQ: this is weak, assumes bs == inp
              Left e -> (# Left e, n# #)
              Right a -> case unGetBits $ f a of
                Left e -> (# Left e, n# #)
                Right l -> let !(# h, x# #) = l n# inp
                           in (# h inp, x# #)
        in (# const gr, nr# #)

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail = GetBits . Left
#endif

instance MonadFail GetBits where
  {-# INLINE fail #-}
  fail = GetBits . Left

instance Alternative GetBits where
  {-# INLINE empty #-}
  empty   = GetBits $ Left "GetBits is empty!"

  {-# INLINE (<|>) #-}
  a <|> b = GetBits
            $ case unGetBits a of
                Right k ->
                  Right $ \ !pos# inp ->
                            let !(# g, n# #) = k pos# inp
                                !(# gr, nr# #) = case g inp of
                                  Right x -> (# Right x, n# #)
                                  Left _ -> case unGetBits b of
                                    Left e -> (# Left e, n# #)
                                    Right l -> let !(# h, m# #) = l pos# inp
                                               in (# h inp, m# #)
                            in (# const gr, nr# #)
                Left _ -> unGetBits b

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

-- extractFromByteString bitLimit startBit numBits bs =
--   let Bytes' s8 = fst (bitsToBytes startBit)
--       Bytes' r8 = fst (bitsToBytes numBits)
--       rcnt = r8 + 2 -- 2 == pre-shift overflow byte on either side

--       -- Extract the relevant bits from the ByteCode, with padding to byte
--       -- boundaries into ws.
--       ws = BS.take rcnt $ BS.drop s8 bs

--       -- Combine the extracted bytes into an Integer value in wi.
--       wi = BS.foldr (\w a -> a `shiftL` 8 .|. fromIntegral w) (0::Int) ws

--       -- Mask is 0-bit based set of bits wanted in the result
--       mask = ((1::Int) `shiftL` bitCount numBits) - 1

--       -- Shift the desired value down to byte alignment and then discard any
--       -- excess high bits.
--       vi = wi `shiftR` (bitCount startBit .&. 7) .&. mask

--       updPos = addBitCounts startBit numBits
--   in if updPos > bitLimit
--      then Left ("Attempt to read bits past limit (newPos="
--                 <> show updPos <> ", limit=" <> show bitLimit <> ")"
--                )
--      else Right (vi, updPos)

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
                getB# !i# =
                  case i# of
                    0# -> let !(W8# w#) = bs `BS.index` (I# s8#)
                          in word2Int# w#
                    _ -> let !(W8# w#) = (bs `BS.index` (I# (s8# +# i#)))
                         in (word2Int# w#) `uncheckedIShiftL#` (8# *# i#)
                getSB# !i# =
                  case i# of
                    0# -> let !(W8# w#) = bs `BS.index` (I# s8#)
                          in (word2Int# w#) `uncheckedIShiftRL#` hop#
                    _  -> let !(W8# w#) = bs `BS.index` (I# (s8# +# i#))
                              !shft# = (8# *# i#) -# hop#
                          in (word2Int# w#) `uncheckedIShiftL#` shft#
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
                                  _ -> let join !(W8# w#) !(I# a#) =
                                             I# ((a# `uncheckedIShiftL#` 8#)
                                                 `orI#` (word2Int# w#))
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
                                 _ -> let join !(W8# w#) !(I# a#) =
                                            I# ((a# `uncheckedIShiftL#` 8#)
                                                `orI#` (word2Int# w#))
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
align32bits  = GetBits $ pure $ \ !pos# inp ->
  let !(# curBit#, ttlBits# #) = pos#
      !s32# = curBit# `andI#` 31#
      !r32# = 32# -# s32#  -- num bits to reach next 32-bit boundary
      nonZero = "alignments @" <> show (I# curBit#)
                <> " not zeroes up to 32-bit boundary"
  in if isTrue# (s32# ==# 0#)
     then (# const $ Right (), pos# #)
     else case extractFromByteString ttlBits# curBit# r32# inp of
            Right getRes ->
              let !(# vi#, newPos# #) = getRes ()
              in if isTrue# (vi# ==# 0#)
                 then (# const $ Right (), (# newPos#, ttlBits# #) #)
                 else (# const $ Left nonZero, pos# #)
            Left e -> (# const $ Left e, pos# #)


-- | Read out n bits as a @BitString@.
fixed :: NumBits -> GetBits BitString
fixed !(Bits' (I# n#)) = GetBits $ pure
  $ \ !s@(# cur#, lim# #) ->
      \inp ->
        case extractFromByteString lim# cur# n# inp of
          Right getRes ->
            let !(# v#, p# #) = getRes ()
            in (# const $ pure $ toBitString (Bits' (I# n#)) (I# v#)
               , (# p#, lim# #)
               #)
          Left e -> (# const $ Left e, s #)


-- | Read out n bytes as a @ByteString@, aligning to a 32-bit boundary before and
-- after.
bytestring :: NumBytes -> GetBits ByteString
bytestring n@(Bytes' nbytes) = do
  align32bits
  r <- GetBits $ pure
       $ \ !(# pos#, lim# #) ->
           \inp ->
             let !sbyte# = pos# `uncheckedIShiftRL#` 3# -- known to be aligned
                 !endAt# = pos# +# bitCount# (bytesToBits n)
                 !end# = (# endAt#, lim# #)
                 err = "Sub-bytestring attempted beyond end of input bytestring"
             in if isTrue# (endAt# <=# lim#)
                then (# const $ pure $ BS.take nbytes $ BS.drop (I# sbyte#) inp, end# #)
                else (# const $ Left err, end# #)
  align32bits
  return r


-- | Add a label to the error tag stack.
label :: String -> GetBits a -> GetBits a
label l m = case unGetBits m of
              Left e -> GetBits $ Left $ e <> "\n  " <> l
              Right k -> GetBits $ pure
                         $ \ !pos# inp ->
                             let !(# j, n# #) = k pos# inp
                             in case j inp of
                                  Left e -> (# const $ Left $ e <> "\n  " <> l, n# #)
                                  Right r -> (# const $ Right r, n# #)


-- | Isolate input to a sub-span of the specified byte length.
isolate :: NumBytes -> GetBits a -> GetBits a
isolate ws m =
  case unGetBits m of
    Right k -> GetBits $ pure
               $ \ !(# pos#, lim# #) ->
                   \inp ->
                     let !l# = pos# +# bitCount# (bytesToBits ws)
                     in let !(# r, (# x#, _ #) #) = k (# pos#, l# #) inp
                        in (# r, (# x#, lim# #) #)
    Left e -> GetBits $ Left e


-- | Try to parse something, returning Nothing when it fails.

try :: GetBits a -> GetBits (Maybe a)
try m = (Just <$> m) `mplus` return Nothing


-- | Skips the specified number of bits

skip :: NumBits -> GetBits ()
skip !(Bits' (I# n#)) = GetBits $ pure
  $ \ !(# cur#, lim# #) ->
      let !newLoc# = cur# +# n#
          !newPos# = (# newLoc#, lim# #)
      in if isTrue# (newLoc# ># lim#)
         then \_ -> (# const $ Left "skipped past end of bytestring"
                    , newPos#
                      #)
         else \_ -> (# const $ Right (), newPos# #)
