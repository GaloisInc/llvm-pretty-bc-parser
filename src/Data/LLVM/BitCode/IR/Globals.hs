{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Globals where

import Data.LLVM.BitCode.IR.Attrs
import Data.LLVM.BitCode.IR.Values
import Data.LLVM.BitCode.Record
import Data.LLVM.BitCode.Parse
import Text.LLVM.AST
import Text.LLVM.Labels

import Control.Monad (guard,mplus)
import Data.Bits (bit,shiftR,testBit)
import qualified Data.Sequence as Seq
import Data.Word (Word32)


-- Global Variables ------------------------------------------------------------

type GlobalList = Seq.Seq PartialGlobal

data PartialGlobal = PartialGlobal
  { pgSym     :: Symbol
  , pgAttrs   :: GlobalAttrs
  , pgType    :: Type
  , pgValueIx :: Maybe Int
  , pgAlign   :: Maybe Align
  } deriving Show

-- [ pointer type, isconst, initid
-- , linkage, alignment, section, visibility, threadlocal
-- , unnamed_addr
-- ]
parseGlobalVar :: Int -> Record -> Parse PartialGlobal
parseGlobalVar n r = label "GLOBALVAR" $ do
  let field = parseField r
  ptrty   <- getType =<< field 0 numeric
  mask    <-             field 1 numeric
  let isconst    = testBit (mask :: Word32) 0
      explicitTy = testBit  mask            1
  initid  <-             field 2 numeric
  link    <-             field 3 linkage

  mbAlign <- if length (recordFields r) > 4
                then Just `fmap` field 4 numeric
                else return Nothing

  ty <- if explicitTy
           then return ptrty
           else elimPtrTo ptrty `mplus` fail "Invalid type for value"

  name    <- entryName n
  _       <- pushValue (Typed ptrty (ValSymbol (Symbol name)))
  let valid | initid == 0 = Nothing
            | otherwise   = Just (initid - 1)
      attrs = GlobalAttrs
        { gaLinkage    = do
          guard (link /= External)
          return link
        , gaConstant   = isconst
        }

  return PartialGlobal
    { pgSym     = Symbol name
    , pgAttrs   = attrs
    , pgType    = ty
    , pgValueIx = valid
    , pgAlign   = do
        b <- mbAlign
        let aval = bit b `shiftR` 1
        guard (aval > 0)
        return aval
    }

finalizeGlobal :: PartialGlobal -> Parse Global
finalizeGlobal pg = case pgValueIx pg of
  Nothing -> return (mkGlobal ValNull)
  Just ix -> do
    tv <- getFnValueById (pgType pg) (fromIntegral ix)
    mkGlobal `fmap` relabel (const requireBbEntryName) (typedValue tv)
  where
  mkGlobal val = Global
    { globalSym   = pgSym pg
    , globalAttrs = pgAttrs pg
    , globalType  = pgType pg
    , globalValue = Just val
    , globalAlign = pgAlign pg
    }
