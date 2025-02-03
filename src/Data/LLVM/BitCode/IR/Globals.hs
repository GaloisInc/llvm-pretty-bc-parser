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
import qualified Data.Map as Map
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
  , pgMd      :: Map.Map KindMd PValMd
  } deriving Show

-- [ pointer type, isconst, initid
-- , linkage, alignment, section, visibility, threadlocal
-- , unnamed_addr
-- ]
parseGlobalVar :: Int -> Record -> Parse PartialGlobal
parseGlobalVar n r = label "GLOBALVAR" $ do
  (name, offset) <- oldOrStrtabName n r
  let field i = parseField r (i + offset)
  ptrty   <- getType =<< field 0 numeric
  mask    <-             field 1 numeric
  let isconst    = testBit (mask :: Word32) 0
      explicitTy = testBit  mask            1
  initid  <-             field 2 numeric
  link    <-             field 3 linkage

  mbAlign <- if length (recordFields r) > (4 + offset)
                then Just `fmap` field 4 numeric
                else return Nothing
  vis <- if length (recordFields r) > (6 + offset) && not (link `elem` [Internal, Private])
                then field 6 visibility
                else pure DefaultVisibility
  tl <- if length (recordFields r) > (7 + offset) -- && not (link `elem` [Internal, Private])
                then field 7 threadLocal
                else pure NotThreadLocal

  unnamed <-
    if length (recordFields r) > (8 + offset)
    then do
      field 8 unnamedAddr
    else return Nothing

  addrspace <- if explicitTy
                  then return . AddrSpace . fromIntegral $ shiftR mask 2
                  else case ptrty of
                         PtrTo as _ -> return as
                         PtrOpaque as -> return as
                         _ -> fail $ "Invalid type for value: " ++ show ptrty

  ty <- if explicitTy
           then return ptrty
           else elimPtrTo ptrty `mplus` (fail $ "Invalid type for value: " ++ show ptrty)

  _       <- pushValue (Typed (PtrTo addrspace ty) (ValSymbol name))
  let valid | initid == 0 = Nothing
            | otherwise   = Just (initid - 1)
      attrs = GlobalAttrs
        { gaLinkage     = Just link
        , gaVisibility  = Just vis
        , gaThreadLocality = Just tl
        , gaUnnamedAddr = unnamed
        , gaConstant    = isconst
        , gaAddrSpace   = addrspace
        }

  return PartialGlobal
    { pgSym     = name
    , pgAttrs   = attrs
    , pgType    = ty
    , pgValueIx = valid
    , pgAlign   = do
        b <- mbAlign
        let aval = bit b `shiftR` 1
        guard (aval > 0)
        return aval
    , pgMd      = Map.empty
    }

finalizeGlobal :: PartialGlobal -> Finalize Global
finalizeGlobal pg = case pgValueIx pg of
  Nothing -> mkGlobal Nothing
  Just ix -> do
    tv <- getFnValueById (pgType pg) (fromIntegral ix)
    val <- relabel requireBbEntryName (typedValue tv)
    mkGlobal (Just val)
  where
  mkGlobal mval =
    do md <- mapM (relabel requireBbEntryName) (pgMd pg)
       return Global { globalSym   = pgSym pg
                     , globalAttrs = pgAttrs pg
                     , globalType  = pgType pg
                     , globalValue = mval
                     , globalAlign = pgAlign pg
                     , globalMetadata = md
                     }


setGlobalMetadataAttachment ::
  Map.Map KindMd PValMd ->
  (PartialGlobal -> PartialGlobal)
setGlobalMetadataAttachment pmd pg = pg { pgMd = pmd }
