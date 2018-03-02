{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Values (
    getValueTypePair
  , getConstantFwdRef, getConstantFwdRefAdjustedId 
  , getValue
  , getFnValueById, getFnValueById'
  , parseValueSymbolTableBlock
  ) where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST

import Control.Monad ((<=<),foldM)
import qualified Data.Map as Map


-- Value Table -----------------------------------------------------------------

-- | Return a forward reference if the value is not in the incremental table.
getConstantFwdRef :: ValueTable -> Type -> Int -> Parse (Typed PValue)
getConstantFwdRef t ty n = label "getConstantFwdRef" $
    adjustId n >>= getConstantFwdRefAdjustedId t ty

getConstantFwdRefAdjustedId :: ValueTable -> Type -> Int -> Parse (Typed PValue)
getConstantFwdRefAdjustedId t ty n' = label "getConstantFwdRefAdjustedId" $ do
  mb <- lookupValue n'
  case mb of
    Just tv -> return tv

    -- forward reference
    Nothing -> do
      cxt <- getContext
      let ref = forwardRef cxt n' t
      return (Typed ty (typedValue ref))

-- | Get either a value from the value table, with its value, or parse a value
-- and a type.
getValueTypePair :: ValueTable -> Record -> Int -> Parse (Typed PValue, Int)
getValueTypePair t r ix = do
  let field = parseField r
  n  <- adjustId =<< field ix numeric
  mb <- lookupValueAbs n
  case mb of

    -- value is already present in the incremental table
    Just tv -> return (tv, ix+1)

    -- forward reference to the entry in the final table
    Nothing -> do
      ty  <- getType =<< field (ix+1) numeric
      cxt <- getContext
      let ref = forwardRef cxt n t

      -- generate the forward reference to the value only, as we already know
      -- what the type should be.
      return (Typed ty (typedValue ref), ix+2)

-- | Get a single value from the value table.
getValueNoFwdRef :: Type -> Int -> Parse (Typed PValue)
getValueNoFwdRef ty n = label "getValueNoFwdRef" (getFnValueById ty =<< adjustId n)

getFnValueById :: Type -> Int -> Parse (Typed PValue)
getFnValueById  = getFnValueById' Nothing

getValue :: ValueTable -> Type -> Int -> Parse (Typed PValue)
getValue vt ty n = label "getValue" (getFnValueById' (Just vt) ty =<< adjustId n)

-- | Lookup a value by its absolute id, or perhaps some metadata.
getFnValueById' :: Maybe ValueTable -> Type -> Int -> Parse (Typed PValue)
getFnValueById' mbVt ty n = label "getFnValueById'" $ case ty of

  PrimType Metadata -> do
    cxt <- getContext
    md  <- getMdTable
    return (forwardRef cxt n md)

  _ -> do
    mb <- lookupValueAbs n
    case mb of

      Just tv -> return tv

      -- forward reference
      Nothing -> do
        mb <- entryNameMb n
        case mb of
          Just name -> return (Typed ty (ValIdent (Ident name)))

          Nothing
            | Just vt <- mbVt ->
              do cxt <- getContext
                 return (forwardRef cxt n vt)

            | otherwise ->
              fail "Unable to create forward reference"

-- Value Symbol Table Entries --------------------------------------------------

vstCodeEntry :: Match Entry Record
vstCodeEntry  = hasRecordCode 1 <=< fromEntry

vstCodeBBEntry :: Match Entry Record
vstCodeBBEntry  = hasRecordCode 2 <=< fromEntry

vstCodeFNEntry :: Match Entry Record
vstCodeFNEntry  = hasRecordCode 3 <=< fromEntry

-- Value Symbol Table Parsing --------------------------------------------------

parseValueSymbolTableBlock :: [Entry] -> Parse ValueSymtab
parseValueSymbolTableBlock  = foldM parseValueSymbolTableBlockEntry Map.empty

parseValueSymbolTableBlockEntry :: ValueSymtab -> Entry -> Parse ValueSymtab

parseValueSymbolTableBlockEntry vs (vstCodeEntry -> Just r) = do
  -- VST_ENTRY: [valid, namechar x N]
  let field = parseField r
  valid <- field 0 numeric
  name  <- field 1 cstring
  return (addEntry valid name vs)

parseValueSymbolTableBlockEntry vs (vstCodeBBEntry -> Just r) = do
  -- VST_BBENTRY: [bbid, namechar x N]
  let field = parseField r
  bbid <- field 0 numeric
  name <- field 1 cstring
  return (addBBEntry bbid name vs)

parseValueSymbolTableBlockEntry vs (vstCodeFNEntry -> Just r) = do
  -- VST_FNENTRY: [valid, offset, namechar x N]
  let field = parseField r
  valid  <- field 0 numeric
  offset <- field 1 numeric
  name   <- field 2 cstring
  return (addFNEntry valid offset name vs)

parseValueSymbolTableBlockEntry vs (abbrevDef -> Just _) =
  -- skip abbreviation definitions, they're already resolved
  return vs

parseValueSymbolTableBlockEntry vs (block -> Just _) =
  -- skip blocks, there are no known subblocks.
  return vs

parseValueSymbolTableBlockEntry _ e =
  fail ("value symtab: unexpected entry: " ++ show e)
