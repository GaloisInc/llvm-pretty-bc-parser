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

import           Data.LLVM.BitCode.BitString

import           Control.Applicative ( Alternative(..) )
import           Control.Monad ( MonadPlus(..) )
import           Data.Bits ( (.&.), (.|.), shiftL, shiftR )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as BS

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail ( MonadFail )
import qualified Control.Monad.Fail
#endif

-- Bit-level Parsing -----------------------------------------------------------

newtype GetBits a = GetBits { unGetBits :: Either String (BitPosition -> BS.ByteString -> (BitsGetter a, BitPosition)) } -- Left is outer fail

newtype BitPosition = BitPosition (NumBits, NumBits) -- (current, limit)

type BitsGetter a = BS.ByteString -> Either String a -- Left is inner fail


-- | Run a @GetBits@ action, returning its value, and the number of bits offset
-- into the next byte of the stream.
runGetBits :: GetBits a -> ByteString -> Either String a
runGetBits m bs =
  let startPos = BitPosition ( Bits' 0
                             , bytesToBits $ Bytes' $ BS.length bs
                             )
  in ($ bs) =<< (fst . ($ bs) . ($ startPos) <$> unGetBits m)


instance Functor GetBits where
  {-# INLINE fmap #-}
  fmap f m = GetBits $ case unGetBits m of
    Left e -> Left e
    Right g -> Right $ \pos inp -> let (b,n) = g pos inp
                                   in (\bs -> f <$> b bs, n)

instance Applicative GetBits where
  {-# INLINE pure #-}
  pure x = GetBits $ pure $ \pos _ -> (const $ pure x, pos)

  {-# INLINE (<*>) #-}
  f <*> x =
    GetBits $ do k <- unGetBits f
                 l <- unGetBits x
                 return $ \pos inp -> let (g,n) = k pos inp
                                          (y,m) = l n inp
                                          in ( \_ -> do g' <- g inp
                                                        y' <- y inp
                                                        return $ g' y'
                                             , m
                                             )

instance Monad GetBits where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  m >>= f =
    case unGetBits m of
      Left e -> GetBits $ Left e
      Right k -> GetBits $ pure $ \pos inp ->
        let (g,n) = k pos inp
            (gr,nr) = case g inp of  -- KWQ: this is weak, assumes bs == inp
              Left e -> (Left e, n)
              Right a -> case unGetBits $ f a of
                Left e -> (Left e, n)
                Right l -> let (h,x) = l n inp
                           in (h inp, x)
        in (const gr, nr)

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
                  Right $ \pos inp ->
                            let (g,n) = k pos inp
                                (gr,nr) = case g inp of
                                  Right x -> (Right x, n)
                                  Left _ -> case unGetBits b of
                                    Left e -> (Left e, n)
                                    Right l -> let (h,m) = l pos inp
                                               in (h inp, m)
                            in (const gr, nr)
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

extractFromByteString :: NumBits -> NumBits -> NumBits -> ByteString
                      -> Either String (Integer, NumBits)
extractFromByteString bitLimit startBit numBits bs =
  let Bytes' s8 = fst (bitsToBytes startBit)
      Bytes' r8 = fst (bitsToBytes numBits)
      rcnt = r8 + 2 -- 2 == pre-shift overflow byte on either side
      ws = BS.take rcnt $ BS.drop s8 bs
      wi = BS.foldr (\w a -> a `shiftL` 8 .|. fromIntegral w) (0::Integer) ws
      mask = ((1::Integer) `shiftL` bitCount numBits) - 1
      vi = wi `shiftR` (bitCount startBit .&. 7) .&. mask
      updPos = addBitCounts startBit numBits
  in if updPos > bitLimit
     then Left ("Attempt to read bits past limit (newPos="
                <> show updPos <> ", limit=" <> show bitLimit <> ")"
               )
     else Right (vi, updPos)


-- Basic Interface -------------------------------------------------------------

-- | Read zeros up to an alignment of 32-bits.
align32bits :: GetBits ()
align32bits  = GetBits $ pure $ \pos inp ->
  let BitPosition (curBit, ttlBits) = pos
      s32 = bitCount curBit .&. (32 - 1)
      r32 = Bits' $ 32 - s32  -- num bits to reach next 32-bit boundary
      nonZero = "alignments @" <> show curBit
                <> " not zeroes up to 32-bit boundary"
  in if s32 == 0
     then (const $ Right (), pos)
     else case extractFromByteString ttlBits curBit r32 inp of
            Right (vi, newPos) ->
              if vi == 0
              then (const $ Right (), BitPosition (newPos, ttlBits))
              else (const $ Left nonZero, pos)
            Left e -> (const $ Left e, pos)


-- | Read out n bits as a @BitString@.
fixed :: NumBits -> GetBits BitString
fixed n = GetBits $ pure
          $ \s@(BitPosition (cur,lim)) ->
              \inp ->
                case extractFromByteString lim cur n inp of
                  Right (v,p) -> ( const $ pure $ BitString n v
                                 , BitPosition (p,lim)
                                 )
                  Left e -> (const $ Left e, s)


-- | Read out n bytes as a @ByteString@, aligning to a 32-bit boundary before and
-- after.
bytestring :: NumBytes -> GetBits ByteString
bytestring n@(Bytes' nbytes) = do
  align32bits
  r <- GetBits $ pure
       $ \(BitPosition (pos,lim)) ->
           \inp ->
             let Bytes' sbyte = fst $ bitsToBytes pos   -- aligned, so snd == 0
                 endAt = pos `addBitCounts` bytesToBits n
                 end = BitPosition (endAt, lim)
                 err = "Sub-bytestring attempted beyond end of input bytestring"
             in if endAt <= lim
                then (const $ pure $ BS.take nbytes $ BS.drop sbyte inp, end)
                else (const $ Left err, end)
  align32bits
  return r


-- | Add a label to the error tag stack.
label :: String -> GetBits a -> GetBits a
label l m = case unGetBits m of
              Left e -> GetBits $ Left $ e <> "\n  " <> l
              Right k -> GetBits $ pure
                         $ \pos inp ->
                             let (j,n) = k pos inp
                             in case j inp of
                                  Left e -> (const $ Left $ e <> "\n  " <> l, n)
                                  Right r -> (const $ Right r, n)


-- | Isolate input to a sub-span of the specified byte length.
isolate :: NumBytes -> GetBits a -> GetBits a
isolate ws m =
  case unGetBits m of
    Right k -> GetBits $ pure
               $ \(BitPosition (pos, lim)) ->
                   \inp ->
                     let l = pos `addBitCounts` bytesToBits ws
                     in let (r,BitPosition (x, _)) = k (BitPosition (pos, l)) inp
                        in (r, BitPosition (x, lim))
    Left e -> GetBits $ Left e


-- | Try to parse something, returning Nothing when it fails.

try :: GetBits a -> GetBits (Maybe a)
try m = (Just <$> m) `mplus` return Nothing


-- | Skips the specified number of bits

skip :: NumBits -> GetBits ()
skip n = GetBits $ pure
         $ \(BitPosition (cur,lim)) ->
             let newLoc = addBitCounts cur n
                 newPos = BitPosition (newLoc, lim)
             in if newLoc > lim
                then const ( const $ Left "skipped past end of bytestring"
                           , newPos
                           )
                else const (const $ Right (), newPos)
