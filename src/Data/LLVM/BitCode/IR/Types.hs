{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Types (
    resolveTypeDecls
  , parseTypeBlock
  ) where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST

import Control.Monad (when,unless,mplus,(<=<))
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Map as Map


-- Type Block ------------------------------------------------------------------

-- | Pattern match the TYPE_CODE_NUMENTRY unabbreviated record.
numEntry :: Match Entry Record
numEntry  = hasRecordCode 1 <=< fromUnabbrev <=< unabbrev

resolveTypeDecls :: Parse [TypeDecl]
resolveTypeDecls  = do
  symtab <- getTypeSymtab
  decls  <- mapM mkTypeDecl (Map.toList (tsById symtab))
  return (sortBy (comparing typeName) decls)
  where
  mkTypeDecl (ix,alias) = do
    ty <- getType' ix
    return TypeDecl
      { typeName  = alias
      , typeValue = ty
      }


-- Type Block Parsing ----------------------------------------------------------

-- | Parsing the type block only modifies internal state, introducing a number
-- of entries to the type table.
parseTypeBlock :: [Entry] -> Parse TypeSymtab
parseTypeBlock es = label "TYPE_BLOCK" $ do

  -- drop everything until we hit TYPE_CODE_NUMENTRY
  (r,ents) <- match (dropUntil numEntry) es
  setTypeTableSize =<< label "type-table size" (parseField r 0 numeric)

  -- verify that the type table hasn't been set already
  isEmpty <- isTypeTableEmpty
  unless isEmpty (fail "Multiple TYPE_BLOCKs found!")

  -- resolve the type table, and the type symbol table
  tys <- mapM parseTypeBlockEntry ents
  cxt <- getContext
  let (tt,sym) = deriveTypeTables cxt (catMaybes tys)
  setTypeTable tt
  return sym

deriveTypeTables :: [String] -> [(PType,Maybe Ident)] -> (TypeTable,TypeSymtab)
deriveTypeTables cxt tys = (tt,sym)
  where
  ixs = zip [0 ..] tys

  -- symbol table entries aren't very common
  sym = foldl mkSym mempty ixs
  mkSym sym' (ix,(_,mb)) = case mb of
    Nothing    -> sym'
    Just alias -> addTypeSymbol ix alias sym'

  -- recursively resolve the type table, if they don't already exist in the
  -- symbol table.  if the index entry doesn't exist, throw an error, as that
  -- should be impossible.
  tt = Map.fromList [ (ix,updateAliases resolve ty) | (ix,(ty,_)) <- ixs ]
  resolve ix = case Map.lookup ix (tsById sym) of
    Nothing    -> lookupTypeRef cxt ix tt
    Just ident -> Alias ident


type PType = Type' Int

type ParseType = Parse (Maybe (PType,Maybe Ident))

typeRef :: Match Field PType
typeRef  = return . Alias <=< numeric

-- | Parsing the type table will only ever effect internal state.
parseTypeBlockEntry :: Entry -> ParseType

parseTypeBlockEntry (fromEntry -> Just r) = case recordCode r of

  1 -> label "TYPE_CODE_NUMENTRY" noType

  2 -> label "TYPE_CODE_VOID" (addType (PrimType Void))

  3 -> label "TYPE_CODE_FLOAT" (addType (PrimType (FloatType Float)))

  4 -> label "TYPE_CODE_DOUBLE" (addType (PrimType (FloatType Double)))

  5 -> label "TYPE_CODE_LABEL" (addType (PrimType Label))

  6 -> label "TYPE_CODE_OPAQUE" $ do
    do ident    <- getTypeName
       addTypeWithAlias Opaque ident

  7 -> label "TYPE_CODE_INTEGER" $ do
    let field = parseField r
    width <- field 0 numeric
    addType (PrimType (Integer width))

  8 -> label "TYPE_CODE_POINTER" $ do
    let field = parseField r
    ty <- field 0 typeRef
    when (length (recordFields r) == 2) $ do
      _space <- field 1 keep
      return ()
    addType (PtrTo ty)

  -- [vararg, attrid, [retty, paramty x N]]
  9 -> label "TYPE_CODE_FUNCTION_OLD" $ do
    let field = parseField r
    va  <- field 0 boolean
    tys <- field 2 (fieldArray typeRef)
    case tys of
      rty:ptys -> addType (FunTy rty ptys va)
      _        -> fail "function expects a return type"

  10 -> label "TYPE_CODE_X86_FP80" (addType (PrimType (FloatType Half)))

  11 -> label "TYPE_CODE_ARRAY" $ do
    let field = parseField r
    numelts <- field 0 numeric
    eltty   <- field 1 typeRef
    addType (Array numelts eltty)

  12 -> label "TYPE_CODE_VECTOR" $ do
    let field = parseField r
    numelts <- field 0 numeric
    eltty   <- field 1 typeRef
    addType (Vector numelts eltty)

  13 -> label "TYPE_CODE_X86_FP80" (addType (PrimType (FloatType X86_fp80)))

  14 -> label "TYPE_CODE_FP128" (addType (PrimType (FloatType Fp128)))

  15 -> label "TYPE_CODE_PPC_FP128" (addType (PrimType (FloatType PPC_fp128)))

  16 -> label "TYPE_CODE_METADATA" (addType (PrimType Metadata))

  17 -> label "TYPE_CODE_X86_MMX" (addType (PrimType X86mmx))

  -- [ispacked, eltty x N]
  18 -> label "TYPE_CODE_STRUCT_ANON" $ do
    let field = parseField r
    ispacked <- label "is packed"     (field 0 boolean)
    tys      <- label "struct fields" (field 1 (fieldArray typeRef))
    if ispacked
       then addType (PackedStruct tys)
       else addType (Struct tys)

  19 -> label "TYPE_CODE_STRUCT_NAME" $ do
    name <- label "struct name" $ parseField r 0 cstring
        `mplus` parseFields r 0 char
    setTypeName name
    noType

  -- [ispacked, eltty x N]
  20 -> label "TYPE_CODE_STRUCT_NAMED" $ do
    let field = parseField r

    ident    <- getTypeName
    ispacked <- label "ispacked"      (field 0 boolean)
    tys      <- label "element types" (field 1 (fieldArray typeRef))
    if ispacked
       then addTypeWithAlias (PackedStruct tys) ident
       else addTypeWithAlias (Struct tys) ident

  -- [vararg, [retty, paramty x N]]
  21 -> label "TYPE_CODE_FUNCTION" $ do
    let field = parseField r
    vararg <- label "vararg"     (field 0 boolean)
    tys    <- label "parameters" (field 1 (fieldArray typeRef))
    case tys of
      rty:ptys -> addType (FunTy rty ptys vararg)
      []       -> fail "function expects a return type"

  code -> fail ("unknown type code " ++ show code)

-- skip blocks
parseTypeBlockEntry (block -> Just _) =
  return Nothing

-- skip abbrevs
parseTypeBlockEntry (abbrevDef -> Just _) =
  return Nothing

parseTypeBlockEntry e =
  fail ("type block: unexpected: " ++ show e)



-- | Add a type to the type table.
addType :: PType -> ParseType
addType ty = return (Just (ty,Nothing))

-- | Add a type and an alias to the type table
addTypeWithAlias :: PType -> Ident -> ParseType
addTypeWithAlias ty i = return (Just (ty,Just i))

-- | Return no type for addition to the type table
noType :: ParseType
noType  = return Nothing
