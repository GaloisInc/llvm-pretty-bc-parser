module Data.LLVM.BitCode.Record where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.BitString hiding (drop,take)
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse

import Data.Bits (Bits,testBit,shiftR,bit)
import Data.Char (chr)
import Control.Monad ((<=<),MonadPlus(..),guard)


-- Generic Records -------------------------------------------------------------

data Record = Record
  { recordCode   :: !Int
  , recordFields :: [Field]
  } deriving (Show)

fromEntry :: Match Entry Record
fromEntry  = (fromUnabbrev <=< unabbrev) ||| (fromAbbrev <=< abbrev)

-- | Record construction from an unabbreviated record
fromUnabbrev :: Match UnabbrevRecord Record
fromUnabbrev u = return Record
  { recordCode   = unabbrevCode u
  , recordFields = map FieldLiteral (unabbrevOps u)
  }

-- | Record construction from an abbreviated field.
fromAbbrev :: Match AbbrevRecord Record
fromAbbrev a = do
  guard (not (null (abbrevFields a)))
  let (f:fs) = abbrevFields a
  code <- numeric f
  return Record
    { recordCode   = code
    , recordFields = fs
    }

-- | Match the record with the given code.
hasRecordCode :: Int -> Match Record Record
hasRecordCode c r | recordCode r == c = return r
                  | otherwise         = mzero

-- | Get a field from a record
fieldAt :: Int -> Match Record Field
fieldAt n = index n . recordFields

-- | Match a literal field.
fieldLiteral :: Match Field BitString
fieldLiteral (FieldLiteral bs) = return bs
fieldLiteral _                 = mzero

-- | Match a fixed field.
fieldFixed :: Match Field BitString
fieldFixed (FieldFixed bs) = return bs
fieldFixed _               = mzero

-- | Match a vbr field.
fieldVbr :: Match Field BitString
fieldVbr (FieldVBR bs) = return bs
fieldVbr _             = mzero

-- | Match a character field.
fieldChar6 :: Match Field Char
fieldChar6 (FieldChar6 c) = return c
fieldChar6 _              = mzero

-- | Match the array field.
fieldArray :: Match Field a -> Match Field [a]
fieldArray p (FieldArray fs) = mapM p fs
fieldArray _ _               = mzero

type LookupField a = Int -> Match Field a -> Parse a

-- | Parse a field from a record.
parseField :: Record -> LookupField a
parseField r n p = case (p <=< fieldAt n) r of
  Just a  -> return a
  Nothing -> fail $ unwords
    [ "unable to parse record field", show n, "of record", show r ]

-- | Parse all record fields starting from an index.
parseFields :: Record -> Int -> Match Field a -> Parse [a]
parseFields r n = parseSlice r n (length (recordFields r))

-- | Parse out a sublist from a record
parseSlice :: Record -> Int -> Int -> Match Field a -> Parse [a]
parseSlice r l n p = loop (take n (drop l (recordFields r)))
  where
  loop (f:fs) = do
    case p f of
      Just a  -> (a:) `fmap` loop fs
      Nothing -> fail $ unwords
        ["unable to parse record field", show n, "of record", show r]

  loop []     = return []

-- | Parse a @Field@ as a numeric value.
numeric :: Num a => Match Field a
numeric  = fmap fromBitString . (fieldLiteral ||| fieldFixed ||| fieldVbr)

-- | Parse a @Field@ as a sign-encoded number.
signed :: (Bits a, Num a) => Match Field a
signed  = fmap decode . (fieldLiteral ||| fieldFixed ||| fieldVbr)
  where
  decode bs
    | not (testBit n 0) =         n `shiftR` 1
    | n /= 1            = negate (n `shiftR` 1)
    | otherwise         = bit 63 -- not really right, but it's what llvm does
    where
    n = fromBitString bs

boolean :: Match Field Bool
boolean  = decode <=< (fieldFixed ||| fieldLiteral ||| fieldVbr)
  where
  decode bs
    | bsData bs == 1 = return True
    | bsData bs == 0 = return False
    | otherwise      = mzero

char :: Match Field Char
char  = fmap chr . numeric

string :: Match Field String
string  = fieldArray (fmap chr . numeric)

cstring :: Match Field String
cstring  = fieldArray (fieldChar6 ||| char)
