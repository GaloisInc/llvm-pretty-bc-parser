module Data.LLVM.BitCode.Match where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Parse

import Control.Monad (MonadPlus(..),guard)


-- Matching Predicates ---------------------------------------------------------

type Match i a = i -> Maybe a

-- | Run a match in the context of the parsing monad.
match :: Match i a -> i -> Parse a
match p = maybe (fail "match failed") return . p

-- | The match that always succeeds.
keep :: Match i i
keep  = return

-- | The match that always fails.
skip :: Match i a
skip _ = mzero

infixr 8 |||

-- | Try to apply one match, and fall back on the other if it fails.
(|||) :: Match a b -> Match a b -> Match a b
(|||) l r a = l a `mplus` r a

-- | Attempt to apply a match.  This always succeeds, but the result of the
-- match will be a @Maybe@.
tryMatch :: Match a b -> Match a (Maybe b)
tryMatch m = fmap Just . m ||| const (return Nothing)

-- | Require that a list has only one element.
oneChild :: Match [a] a
oneChild [a] = keep a
oneChild _   = mzero

findMatch :: Match a b -> Match [a] (Maybe b)
findMatch p (a:as) = (Just `fmap` p a) `mplus` findMatch p as
findMatch _ []     = return Nothing

-- | Get the nth element of a list.
index :: Int -> Match [a] a
index n as = do
  guard (n < length as)
  return (as !! n)

-- | Drop elements of a list until a predicate matches.
dropUntil :: Match a b -> Match [a] (b,[a])
dropUntil p (a:as) = success `mplus` dropUntil p as
  where
  success = do
    b <- p a
    return (b,as)
dropUntil _ []     = mzero

-- | Require that an @Entry@ be a @Block@.
block :: Match Entry Block
block (EntryBlock b) = keep b
block _              = mzero

-- | Require that an @Entry@ be an @UnabbrevRecord@.
unabbrev :: Match Entry UnabbrevRecord
unabbrev (EntryUnabbrevRecord r) = keep r
unabbrev _                       = mzero

-- | Require that an @Entry@ be an @AbbrevRecord@
abbrev :: Match Entry AbbrevRecord
abbrev (EntryAbbrevRecord r) = keep r
abbrev _                     = mzero

-- | Require than an @Entry@ be a @DefineAbbrev@.
abbrevDef :: Match Entry DefineAbbrev
abbrevDef (EntryDefineAbbrev d) = keep d
abbrevDef _                     = mzero

-- | Require that the block has the provided block id.
hasBlockId :: BlockId -> Match Block Block
hasBlockId bid b | blockId b == bid = keep b
                 | otherwise        = mzero

-- | Require that the unabbreviated record has the provided record id.
hasUnabbrevCode :: RecordId -> Match UnabbrevRecord UnabbrevRecord
hasUnabbrevCode rid r | unabbrevCode r == rid = keep r
                      | otherwise             = mzero
