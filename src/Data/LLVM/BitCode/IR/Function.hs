{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Function where

import qualified Data.LLVM.BitCode.Assert as Assert
import           Data.LLVM.BitCode.Bitstream
import           Data.LLVM.BitCode.IR.Blocks
import           Data.LLVM.BitCode.IR.Constants
import           Data.LLVM.BitCode.IR.Metadata
import           Data.LLVM.BitCode.IR.Values
import           Data.LLVM.BitCode.IR.Attrs
import           Data.LLVM.BitCode.Match
import           Data.LLVM.BitCode.Parse
import           Data.LLVM.BitCode.Record

import           Text.LLVM.AST
import           Text.LLVM.Labels
import           Text.LLVM.PP

import           Control.Monad (when,unless,mplus,mzero,foldM,(<=<))
import           Data.Bits (shiftR,bit,shiftL,testBit,(.&.),(.|.),complement,Bits)
import           Data.Int (Int32)
import           Data.Word (Word32)
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T


-- | When showing a Symbol to the user, show it in the manner that it would appear
-- in LLVM text format.  This displays the Symbol in the format associated with
-- the latest version of LLVM supported by llvm-pretty and this library; the
-- Symbol syntax has not changed from LLVM 3.5 through LLVM 16, and this library
-- is intended to be able to import *any* version of LLVM, so this is not a
-- significant issue that would drive the code changes necessary to make an
-- actual LLVM version available here.

-- NOTE: this cannot be eta-reduced to point-free format because simplified
-- subsumption rules (introduced in GHC 9) requires eta-expansion of some higher
-- order functions in order to maintain soundness and typecheck.
prettySym :: Symbol -> String
prettySym s = show $ ppLLVM llvmVlatest $ llvmPP s


-- Function Aliases ------------------------------------------------------------

type AliasList = Seq.Seq PartialAlias

data PartialAlias = PartialAlias
  { paLinkage    :: Maybe Linkage
  , paVisibility :: Maybe Visibility
  , paName       :: Symbol
  , paType       :: Type
  , paTarget     :: !Word32
  } deriving Show

parseAliasOld :: Int -> Record -> Parse PartialAlias
parseAliasOld n r = do
  sym <- entryName n
  let field = parseField r
      name = Symbol sym
  ty  <- getType   =<< field 0 numeric
  tgt <-               field 1 numeric
  lnk <-               field 2 linkage
  vis <-               field 3 visibility
  _   <- pushValue (Typed ty (ValSymbol name))
  return PartialAlias
    { paLinkage    = Just lnk
    , paVisibility = Just vis
    , paName       = name
    , paType       = ty
    , paTarget     = tgt
    }

parseAlias :: Record -> Parse PartialAlias
parseAlias r = do
  n <- nextValueId
  (name, offset) <- oldOrStrtabName n r
  let field i = parseField r (i + offset)
  ty       <- getType =<< field 0 numeric
  _addrSp  <-             field 1 unsigned
  tgt      <-             field 2 numeric
  lnk      <-             field 3 linkage
  vis      <-             field 4 visibility


  -- XXX: is it the case that the alias type will always be a pointer to the
  -- aliasee?
  _   <- pushValue (Typed (PtrTo ty) (ValSymbol name))

  return PartialAlias
    { paLinkage    = Just lnk
    , paVisibility = Just vis
    , paName       = name
    , paType       = ty
    , paTarget     = tgt
    }

finalizePartialAlias :: PartialAlias -> Finalize GlobalAlias
finalizePartialAlias pa = do
  -- aliases refer to absolute offsets
  tv  <- getFnValueById (paType pa) (fromIntegral (paTarget pa))
  tgt <- relabel requireBbEntryName (typedValue tv)
  return GlobalAlias
    { aliasLinkage    = paLinkage pa
    , aliasVisibility = paVisibility pa
    , aliasName       = paName pa
    , aliasType       = paType pa
    , aliasTarget     = tgt
    }


-- Function Attribute Record ---------------------------------------------------

type DeclareList = Seq.Seq FunProto

-- | Turn a function prototype into a declaration.
finalizeDeclare :: FunProto -> Finalize Declare
finalizeDeclare fp = case protoType fp of
  PtrTo (FunTy ret args va) -> return Declare
    { decLinkage    = protoLinkage fp
    , decVisibility = protoVisibility fp
    , decRetType    = ret
    , decName       = protoSym fp
    , decArgs       = args
    , decVarArgs    = va
    , decAttrs      = []
    , decComdat     = protoComdat fp
    }
  _ -> fail "invalid type on function prototype"


-- Function Body ---------------------------------------------------------------

type DefineList = Seq.Seq PartialDefine

-- | A define with a list of statements for a body, instead of a list of basic
-- bocks.
data PartialDefine = PartialDefine
  { partialLinkage    :: Maybe Linkage
  , partialVisibility :: Maybe Visibility
  , partialGC         :: Maybe GC
  , partialSection    :: Maybe String
  , partialRetType    :: Type
  , partialName       :: Symbol
  , partialArgs       :: [Typed Ident]
  , partialVarArgs    :: Bool
  , partialBody       :: BlockList
  , partialBlock      :: StmtList
  , partialBlockId    :: !Int
  , partialSymtab     :: ValueSymtab
  , partialMetadata   :: Map.Map PKindMd PValMd
  , partialGlobalMd   :: !(Seq.Seq PartialUnnamedMd)
  , partialComdatName :: Maybe String
  } deriving Show

-- | Generate a partial function definition from a function prototype.
emptyPartialDefine :: FunProto -> Parse PartialDefine
emptyPartialDefine proto = do
  (rty,tys,va) <- elimFunPtr (protoType proto)
      `mplus` fail "invalid function type in prototype"
  names <- mapM nameNextValue tys

  symtab <- initialPartialSymtab

  return PartialDefine
    { partialLinkage    = protoLinkage proto
    , partialVisibility = protoVisibility proto
    , partialGC         = protoGC proto
    , partialSection    = protoSect proto
    , partialRetType    = rty
    , partialName       = protoSym proto
    , partialArgs       = zipWith Typed tys names
    , partialVarArgs    = va
    , partialBody       = mempty
    , partialBlock      = mempty
    , partialBlockId    = 0
    , partialSymtab     = symtab
    , partialMetadata   = mempty
    , partialGlobalMd   = mempty
    , partialComdatName = protoComdat proto
    }

-- | Set the statement list in a partial define.
setPartialBlock :: StmtList -> PartialDefine -> PartialDefine
setPartialBlock stmts pd = pd { partialBlock = stmts }

-- | Set the block list in a partial define.
setPartialBody :: BlockList -> PartialDefine -> PartialDefine
setPartialBody blocks pd = pd { partialBody = blocks }

initialPartialSymtab :: Parse ValueSymtab
initialPartialSymtab  = do
  mb <- liftFinalize mempty $ bbEntryName Nothing 0
  case mb of
    Just{}  -> return emptyValueSymtab
    Nothing -> do
      i <- nextResultId
      return (addBBAnon 0 i emptyValueSymtab)

updateLastStmt :: (PStmt -> PStmt) -> PartialDefine -> Parse PartialDefine
updateLastStmt f pd = case updatePartialBlock `mplus` updatePartialBody of
  Just pd' -> return pd'
  Nothing  -> fail "No statement to update"
  where
  updatePartialBlock = updateStmts partialBlock setPartialBlock pd

  updatePartialBody = case Seq.viewr (partialBody pd) of
    blocks Seq.:> b -> do
      b' <- updateStmts partialStmts setPartialStmts b
      return (setPartialBody (blocks Seq.|> b') pd)
    Seq.EmptyR          -> mzero

  updateStmts prj upd a = case Seq.viewr (prj a) of
    stmts Seq.:> stmt -> return (upd (stmts Seq.|> f stmt) a)
    Seq.EmptyR        -> mzero



type BlockLookup = Symbol -> Int -> Finalize BlockLabel

lookupBlockName :: DefineList -> BlockLookup
lookupBlockName dl = lkp
  where
  syms = Map.fromList [ (partialName d, partialSymtab d) | d <- F.toList dl ]
  lkp fn bid = case Map.lookup fn syms of
    Nothing -> fail ("symbol " ++ prettySym fn ++ " is not defined")
    Just st -> case IntMap.lookup bid (bbSymtab st) of
      Nothing -> fail ("block id " ++ show bid ++ " does not exist")
      Just sn -> return (mkBlockLabel sn)

-- | Finalize a partial definition.
finalizePartialDefine :: BlockLookup -> FuncSymTabs -> PartialDefine -> Parse Define
finalizePartialDefine lkp defs pd =
  label "finalizePartialDefine" $
  -- augment the symbol table with implicitly named anonymous blocks, and
  -- generate basic blocks.
  withValueSymtab (partialSymtab pd) $ do
    body <- liftFinalize defs $ finalizeBody lkp (partialBody pd)
    md <- finalizeMetadata defs (partialMetadata pd)
    return Define
      { defLinkage    = partialLinkage pd
      , defVisibility = partialVisibility pd
      , defGC         = partialGC pd
      , defAttrs      = []
      , defRetType    = partialRetType pd
      , defName       = partialName pd
      , defArgs       = partialArgs pd
      , defVarArgs    = partialVarArgs pd
      , defBody       = body
      , defSection    = partialSection pd
      , defMetadata   = md
      , defComdat     = partialComdatName pd
      }

finalizeMetadata :: FuncSymTabs -> PFnMdAttachments -> Parse FnMdAttachments
finalizeMetadata defs patt = Map.fromList <$> mapM f (Map.toList patt)
  where f (k,md) = (,) <$> getKind k <*> (liftFinalize defs $ finalizePValMd md)

-- | Individual label resolution step.
resolveBlockLabel :: BlockLookup -> Maybe Symbol -> Int -> Finalize BlockLabel
resolveBlockLabel lkp mbSym = case mbSym of
  Nothing  -> requireBbEntryName mbSym
  Just sym -> lkp sym

-- | Name the next result with either its symbol, or the next available
-- anonymous result id.
nameNextValue :: Type -> Parse Ident
nameNextValue ty = do
  vs   <- getValueTable
  let nextId = valueNextId vs
  name <- entryName nextId `mplus` (show <$> nextResultId)
  let i   = Ident name
      tv  = Typed ty (ValIdent i)
  setValueTable (addValue tv vs)
  return i

-- | The record that defines the number of blocks in a function.
declareBlocksRecord :: Match Entry UnabbrevRecord
declareBlocksRecord  = hasUnabbrevCode 1 <=< unabbrev

-- | Emit a statement to the current partial definition.
addStmt :: Stmt' Int -> PartialDefine -> Parse PartialDefine
addStmt s d
  | isTerminator (stmtInstr s) = terminateBlock d'
  | otherwise                  = return d'
  where
  d' = d { partialBlock = partialBlock d Seq.|> s }

-- | Terminate the current basic block.  Resolve the name of the next basic
-- block as either its symbol from the symbol table, or the next available
-- anonymous identifier.
terminateBlock :: PartialDefine -> Parse PartialDefine
terminateBlock d = do
  let next = partialBlockId d + 1
  mb <- liftFinalize mempty $ bbEntryName Nothing next
  d' <- case mb of
    Just _  -> return d
    Nothing -> do
      -- no label, use the next result id
      l <- nextResultId
      return d { partialSymtab = addBBAnon next l (partialSymtab d) }

  return d'
    { partialBody  = partialBody d Seq.|> PartialBlock
      { partialLabel = partialBlockId d
      , partialStmts = partialBlock   d
      }
    , partialBlockId = next
    , partialBlock   = Seq.empty
    }

type BlockList = Seq.Seq PartialBlock

-- | Process a @BlockList@, turning it into a list of basic blocks.
finalizeBody :: BlockLookup -> BlockList -> Finalize [BasicBlock]
finalizeBody lkp = fmap F.toList . T.mapM (finalizePartialBlock lkp)

data PartialBlock = PartialBlock
  { partialLabel :: !Int
  , partialStmts :: StmtList
  } deriving (Show)

setPartialStmts :: StmtList -> PartialBlock -> PartialBlock
setPartialStmts stmts pb = pb { partialStmts = stmts }

-- | Process a partial basic block into a full basic block.
finalizePartialBlock :: BlockLookup -> PartialBlock -> Finalize BasicBlock
finalizePartialBlock lkp pb = BasicBlock
                          <$> bbEntryName Nothing (partialLabel pb)
                          <*> finalizeStmts lkp (partialStmts pb)

type PStmt = Stmt' Int

type StmtList = Seq.Seq PStmt

-- | Process a list of statements with explicit block id labels into one with
-- textual labels.
finalizeStmts :: BlockLookup -> StmtList -> Finalize [Stmt]
finalizeStmts lkp = mapM (finalizeStmt lkp) . F.toList

finalizeStmt :: BlockLookup -> Stmt' Int -> Finalize Stmt
finalizeStmt lkp = relabel (resolveBlockLabel lkp)


-- Function Block Parsing ------------------------------------------------------

-- | A bit saying whether a call or callbr instruction has an explicit type, see
-- LLVM's CallMarkersFlags:
-- https://github.com/llvm/llvm-project/blob/c8ed784ee69a7dbdf4b33e85229457ffad309cf2/llvm/include/llvm/Bitcode/LLVMBitCodes.h#L505
callExplicitTypeBit :: Int
callExplicitTypeBit = 15

-- | Parse the function block.
parseFunctionBlock ::
  Int {- ^ unnamed globals so far -} ->
  [Entry] -> Parse PartialDefine
parseFunctionBlock unnamedGlobals ents =
  label "FUNCTION_BLOCK" $ enterFunctionDef $ do

  -- parse the value symtab block first, so that names are present during the
  -- rest of the parse
  symtab <- label "VALUE_SYMTAB" $ do
    mb <- match (findMatch valueSymtabBlockId) ents
    case mb of
      Just es -> parseValueSymbolTableBlock es
      Nothing -> return mempty

  -- pop the function prototype off of the internal stack
  proto <- popFunProto

  label (prettySym (protoSym proto)) $ withValueSymtab symtab $ do

    -- generate the initial partial definition
    pd  <- emptyPartialDefine proto
    rec pd' <- foldM (parseFunctionBlockEntry unnamedGlobals vt) pd ents
        vt  <- getValueTable

    -- merge the symbol table with the anonymous symbol table
    return pd' { partialSymtab = partialSymtab pd' `mappend` symtab }

-- | Parse the members of the function block
parseFunctionBlockEntry ::
  Int {- ^ unnamed globals so far -} ->
  ValueTable -> PartialDefine -> Entry ->
  Parse PartialDefine

parseFunctionBlockEntry _ _ d (constantsBlockId -> Just es) = do
  -- CONSTANTS_BLOCK
  parseConstantsBlock es
  return d

parseFunctionBlockEntry _ t d (fromEntry -> Just r) = case recordCode r of

  -- [n]
  1 -> label "FUNC_CODE_DECLARE_BLOCKS"
             (Assert.recordSizeGreater r 0 >> return d)

  -- [opval,ty,opval,opcode]
  2 -> label "FUNC_CODE_INST_BINOP" $ do
    let field = parseField r
    (lhs,ix) <- getValueTypePair t r 0
    rhs      <- getValue t (typedType lhs) =<< field ix numeric
    mkInstr  <- field (ix + 1) binop
    -- If there's an extra field on the end of the record, it's for one of the
    -- following:
    --
    -- - If the instruction is add, sub, mul, or shl, the extra field
    --   designates the value of the nuw and nsw flags.
    --
    -- - If the instruction is sdiv, udiv, lshr, or ashr, the extra field
    --   designates the value of the exact flag.
    --
    -- - If the instruction is floating-point, the extra field designates the
    --   value of the fast-math flags. We currently ignore these.
    --
    -- The constructor returned from binop will use that value when
    -- constructing the binop.
    let mbWord = numeric =<< fieldAt (ix + 2) r
    result (typedType lhs) (mkInstr mbWord lhs (typedValue rhs)) d

  -- [opval,opty,destty,castopc]
  3 -> label "FUNC_CODE_INST_CAST" $ do
    let field = parseField r
    (tv,ix) <- getValueTypePair t r 0
    Assert.recordSizeIn r [ix + 2]
    resty   <- getType =<< field ix numeric
    cast'   <-             field (ix+1) castOp
    result resty (cast' tv resty) d

  4 -> label "FUNC_CODE_INST_GEP_OLD" (parseGEP t (Just False) r d)

  -- [opval,ty,opval,opval]
  5 -> label "FUNC_CODE_INST_SELECT" $ do
    let field = parseField r
    (tval,ix) <- getValueTypePair t r 0
    fval      <- getValue t (typedType tval)       =<< field  ix    numeric
    cond      <- getValue t (PrimType (Integer 1)) =<< field (ix+1) numeric
    result (typedType tval) (Select cond tval (typedValue fval)) d

  -- [ty,opval,opval]
  6 -> label "FUNC_CODE_INST_EXTRACTELT" $ do
    (tv,ix) <- getValueTypePair t r 0
    idx     <- getValue t (PrimType (Integer 32)) =<< parseField r ix numeric
    (_, ty) <- elimVector (typedType tv)
        `mplus` fail "invalid EXTRACTELT record"
    result ty (ExtractElt tv (typedValue idx)) d

  -- [ty,opval,opval,opval]
  7 -> label "FUNC_CODE_INST_INSERTELT" $ do
    let field = parseField r
    (tv,ix) <- getValueTypePair t r 0
    (_,pty) <- elimVector (typedType tv)
                 `mplus` fail "invalid INSERTELT record (not a vector)"
    elt     <- getValue t pty                     =<< field  ix    numeric
    idx     <- getValue t (PrimType (Integer 32)) =<< field (ix+1) numeric
    result (typedType tv) (InsertElt tv elt (typedValue idx)) d

  -- [opval,ty,opval,opval]
  8 -> label "FUNC_CODE_INST_SHUFFLEVEC" $ do
    let field = parseField r
    (vec1,ix) <- getValueTypePair t r 0
    vec2      <- getValue t (typedType vec1) =<< field  ix    numeric
    (mask,_)  <- getValueTypePair t r (ix+1)
    resTy <- case (typedType vec1,typedType mask) of
                (Vector _ elemTy, Vector shuffleLen _) ->
                        return (Vector shuffleLen elemTy)
                _ -> fail "Invalid arguments to shuffle vector (not vectors)"
    result resTy (ShuffleVector vec1 (typedValue vec2) mask) d

  -- 9 is handled lower down, as it's processed the same way as 28

  -- [opval,opval<optional>]
  10 -> label "FUNC_CODE_INST_RET" $ case length (recordFields r) of
    0 -> effect RetVoid d
    _ -> do
      (tv, ix) <- getValueTypePair t r 0
      Assert.recordSizeIn r [ix]
      effect (Ret tv) d

  -- [bb#,bb#,cond] or [bb#]
  11 -> label "FUNC_CODE_INST_BR" $ do
    Assert.recordSizeIn r [1, 3]
    let field = parseField r
    bb1 <- field 0 numeric
    let jump   = effect (Jump bb1) d
        branch = do
          bb2  <- field 1 numeric
          n    <- field 2 numeric
          cond <- getValue t (PrimType (Integer 1)) n
          effect (Br cond bb1 bb2) d
    branch `mplus` jump

  12 -> label "FUNC_CODE_INST_SWITCH" $ do
    let field = parseField r

    -- switch implementation magic, May 2012 => 1205 => 0x4B5
    let switchInstMagic :: Int
        switchInstMagic  = 0x4B5

    n <- field 0 numeric

    -- parse the new switch format.
    let newSwitch = do
          opty <- getType =<< field 1 numeric

          width <- case opty of
            PrimType (Integer w) -> return w
            _                    -> fail "invalid switch discriminate"

          cond     <- getValue t opty =<< field 2 numeric
          def      <-                      field 3 numeric -- Int id of a label
          numCases <-                      field 4 numeric
          ls       <- parseNewSwitchLabels width r numCases 5
          effect (Switch cond def ls) d



    -- parse the old switch format
    -- [opty, op0, op1, ...]
    let oldSwitch = do
          opty <- getType n
          cond <- getValue t opty =<< field 1 numeric
          def  <-                      field 2 numeric
          ls   <- parseSwitchLabels opty r 3
          effect (Switch cond def ls) d

    -- NOTE: there's a message in BitcodeReader.cpp that indicates that the
    -- newSwitch format is not used as of sometime before 3.4.2.  It's still
    -- supported, but 3.4.2 at least doesn't generate it anymore.
    if n `shiftR` 16 == switchInstMagic then newSwitch else oldSwitch




  -- [attrs,cc,normBB,unwindBB,fnty,op0,op1..]
  13 -> label "FUNC_CODE_INST_INVOKE" $ do
    Assert.recordSizeGreater r 3
    let field = parseField r
    ccinfo      <- field 1 unsigned
    normal      <- field 2 numeric
    unwind      <- field 3 numeric

    -- explicit function type?
    (mbFTy,ix)    <-
      if testBit ccinfo 13
         then do ty <- getType =<< field 4 numeric
                 return (Just ty, 5)
         else return (Nothing, 4)

    (f,ix')      <- getValueTypePair t r ix

    fty <- case mbFTy of
             Just ty -> return ty
             Nothing -> Assert.elimPtrTo "Callee is not a pointer" (typedType f)

    (ret,as,va) <- elimFunTy fty
        `mplus` fail "invalid INVOKE record"

    args        <- parseInvokeArgs t va r ix' as
    -- Use `fty` instead of `typedType f` as the function type, as `typedType f`
    -- will be a pointer type. See Note [Typing function applications].
    result ret (Invoke fty (typedValue f) args normal unwind) d

  14 -> label "FUNC_CODE_INST_UNWIND" (effect Unwind d)

  15 -> label "FUNC_CODE_INST_UNREACHABLE" (effect Unreachable d)

  -- [ty,val0,bb0,...]
  16 -> label "FUNC_CODE_INST_PHI" $ do
    ty     <- getType =<< parseField r 0 numeric

    -- NOTE: we use getRelIds here, as that uses a table that's not currently
    -- stuck in the recursive loop.  Attempting to use valueRelIds on t will
    -- cause a loop.
    useRelIds <- getRelIds
    args      <- parsePhiArgs useRelIds t r

    when (even (length (recordFields r))) $ do
      pure () -- TODO: fast math flags

    result ty (Phi ty args) d

  -- 17 is unused
  -- 18 is unused

  -- [instty,opty,op,align]
  19 -> label "FUNC_CODE_INST_ALLOCA" $ do
    Assert.recordSizeIn r [4]

    let field = parseField r

    instty <- getType           =<< field 0 numeric -- pointer type
    ty     <- getType           =<< field 1 numeric -- size type
    size   <- getFnValueById ty =<< field 2 numeric -- size value
    align  <-                       field 3 numeric -- alignment value

    let sval = case typedValue size of
          ValInteger i | i == 1 -> Nothing
          _                     -> Just size
        mask :: Word32
        mask = (1 `shiftL` 5) .|. -- inalloca
               (1 `shiftL` 6) .|. -- explicit type
               (1 `shiftL` 7)     -- swift error
        aval = (1 `shiftL` (fromIntegral (align .&. complement mask))) `shiftR` 1
        explicitType = testBit align 6
        ity = if explicitType then PtrTo instty else instty

    ret <- if explicitType
           then return instty
           else Assert.elimPtrTo "In return type:" instty

    result ity (Alloca ret sval (Just aval)) d

  -- [opty,op,align,vol]
  20 -> label "FUNC_CODE_INST_LOAD" $ do
    (tv,ix) <- getValueTypePair t r 0
    Assert.recordSizeIn r [ix + 2, ix + 3]

    (ret,ix') <-
      if length (recordFields r) == ix + 3
         then do ty <- getType =<< parseField r ix numeric
                 return (ty,ix+1)

         else do ty <- Assert.elimPtrTo "" (typedType tv)
                 return (ty,ix)

    aval    <- parseField r ix' numeric
    let align | aval > 0  = Just (bit aval `shiftR` 1)
              | otherwise = Nothing
    result ret (Load ret tv Nothing align) d

  -- 21 is unused
  -- 22 is unused

  23 -> label "FUNC_CODE_INST_VAARG" $ do
    Assert.recordSizeGreater r 2
    let field = parseField r
    ty    <- getType     =<< field 0 numeric
    op    <- getValue t ty =<< field 1 numeric
    resTy <- getType     =<< field 2 numeric
    result resTy (VaArg op resTy) d

  -- [ptrty,ptr,val,align,vol]
  24 -> label "FUNC_CODE_INST_STORE_OLD" $ do
    let field = parseField r
    (ptr,ix) <- getValueTypePair t r 0
    ty       <- Assert.elimPtrTo "" (typedType ptr)
    val      <- getValue t ty =<< field ix numeric
    aval     <- field (ix+1) numeric
    let align | aval > 0  = Just (bit aval `shiftR` 1)
              | otherwise = Nothing
    effect (Store val ptr Nothing align) d

  -- 25 is unused

  -- LLVM 6: [opty, opval, n x indices]
  26 -> label "FUNC_CODE_INST_EXTRACTVAL" $ do
    (tv, ix) <- getValueTypePair t r 0

    when (length (recordFields r) == ix) $
      fail "`extractval` instruction had zero indices"

    ixs <- parseIndexes r ix
    let instr = ExtractValue tv ixs

    -- The return type of this instruction depends on the given indices into the
    -- type.
    ret      <- interpValueIndex (typedType tv) ixs
    result ret instr d

  27 -> label "FUNC_CODE_INST_INSERTVAL" $ do
    (tv,ix)   <- getValueTypePair t r 0

    -- See comment in FUNC_CODE_INST_EXTRACTVAL
    when (length (recordFields r) == ix) $
      fail "Invalid instruction with zero indices"

    (elt,ix') <- getValueTypePair t r ix
    ixs       <- parseIndexes r ix'
    result (typedType tv) (InsertValue tv elt ixs) d

  -- 28 is handled lower down, as it's processed the same way as 9

  29 -> label "FUNC_CODE_INST_VSELECT" $ do
    let field = parseField r
    (tv,ix) <- getValueTypePair t r 0
    fv      <- getValue t (typedType tv) =<< field ix numeric
    (c,_)   <- getValueTypePair t r (ix+1)
    -- XXX: we're ignoring the fast-math flags
    result (typedType tv) (Select c tv (typedValue fv)) d

  30 -> label "FUNC_CODE_INST_INBOUNDS_GEP_OLD" (parseGEP t (Just True) r d)

  31 -> label "FUNC_CODE_INST_INDIRECTBR" $ do
    let field = parseField r
    ty   <- getType     =<< field 0 numeric
    addr <- getValue t ty =<< field 1 numeric
    ls   <- parseIndexes r 2
    effect (IndirectBr addr ls) d

  -- 32 is unused

  33 -> label "FUNC_CODE_DEBUG_LOC_AGAIN" $ do
    loc <- getLastLoc
    updateLastStmt (extendMetadata ("dbg", ValMdLoc loc)) d

  -- [paramattrs, cc, mb fmf, mb fnty, fnid, arg0 .. arg n, varargs]
  34 -> label "FUNC_CODE_INST_CALL" $ do
    Assert.recordSizeGreater r 2
    let field = parseField r

    -- pal <- field 0 numeric -- N.B. skipping param attributes
    ccinfo <- field 1 numeric
    let ix0 = if testBit ccinfo 17 then 3 else 2 -- N.B. skipping fast-math flags
    (mbFnTy, ix1) <- if testBit (ccinfo :: Word32) callExplicitTypeBit
                       then do fnTy <- getType =<< field ix0 numeric
                               return (Just fnTy, ix0+1)
                       else    return (Nothing,   ix0)

    (Typed opTy fn, ix2) <- getValueTypePair t r ix1
                                `mplus` fail "Invalid record"

    fnty <- case mbFnTy of
             Just ty -> return ty
             Nothing -> do
               op <- Assert.elimPtrTo "Callee is not a pointer type" opTy
               case op of
                 FunTy{} -> return op
                 _       -> fail "Callee is not of pointer to function type"


    label (show fn) $ do
      (ret,as,va) <- elimFunTy fnty `mplus` fail "invalid CALL record"
      args <- parseCallArgs t va r ix2 as
      -- Use `fnty` instead of `opTy` as the function type, as `opTy` will be
      -- a pointer type. See Note [Typing function applications].
      result ret (Call False fnty fn args) d

  -- [Line,Col,ScopeVal, IAVal]
  35 -> label "FUNC_CODE_DEBUG_LOC" $ do
    Assert.recordSizeGreater r 3

    let field = parseField r
    line    <- field 0 numeric
    col     <- field 1 numeric
    scopeId <- field 2 numeric
    iaId    <- field 3 numeric

    scope <- if scopeId > 0
                then getMetadata (scopeId - 1)
                else fail "No scope provided"

    ia <- if iaId > 0
             then Just `fmap` getMetadata (iaId - 1)
             else return Nothing

    let loc = DebugLoc
          { dlLine  = line
          , dlCol   = col
          , dlScope = typedValue scope
          , dlIA    = typedValue `fmap` ia
          , dlImplicit = False
          }
    setLastLoc loc
    updateLastStmt (extendMetadata ("dbg", ValMdLoc loc)) d

  -- [ordering, synchscope]
  36 -> label "FUNC_CODE_INST_FENCE" $ do
    Assert.recordSizeIn r [2]
    mordval <- getDecodedOrdering =<< parseField r 0 unsigned
    -- TODO: parse scope
    case mordval of
      Just ordval -> effect (Fence Nothing ordval) d
      Nothing -> fail "`fence` instruction requires ordering"

  -- [ptrty,ptr,cmp,new, align, vol,
  --  ordering, synchscope]
  37 -> label "FUNC_CODE_INST_CMPXCHG_OLD" $ do
    notImplemented

  -- LLVM 6.0: [ptrty, ptr, val, operation, vol, ordering, ssid]
  38 -> label "FUNC_CODE_INST_ATOMICRMW_OLD" $
    parseAtomicRMW True t r d


  -- [opval]
  39 -> label "FUNC_CODE_RESUME" $ do
    (tv,_) <- getValueTypePair t r 0
    effect (Resume tv) d

  -- [ty,val,val,num,id0,val0...]
  40 -> label "FUNC_CODE_LANDINGPAD_OLD" $ do
    Assert.recordSizeGreater r 3
    let field = parseField r
    ty          <- getType =<< field 0 numeric
    (persFn,ix) <- getValueTypePair t r 1
    val         <- field ix numeric
    let isCleanup = val /= (0 :: Int)
    len         <- field (ix + 1) numeric
    clauses     <- parseClauses t r len (ix + 2)
    result ty (LandingPad ty (Just persFn) isCleanup clauses) d

  -- [opty, op, align, vol, ordering, synchscope]
  41 -> label "FUNC_CODE_LOADATOMIC" $ do
    (tv,ix) <- getValueTypePair t r 0

    Assert.recordSizeIn r [ix + 4, ix+ 5]

    (ret,ix') <-
      if length (recordFields r) == ix + 5
         then do ty <- getType =<< parseField r ix numeric
                 return (ty, ix + 1)

         else do ty <- Assert.elimPtrTo "" (typedType tv)
                 return (ty, ix)

    ordval <- getDecodedOrdering =<< parseField r (ix' + 2) unsigned
    when (ordval `elem` Nothing:map Just [Release, AcqRel]) $
      fail $ "Invalid atomic ordering: " ++ show ordval

    aval    <- parseField r ix' numeric
    let align | aval > 0  = Just (bit aval `shiftR` 1)
              | otherwise = Nothing

    when (ordval /= Nothing && align == Nothing)
         (fail "Invalid record")

    result ret (Load ret tv ordval align) d


  -- [ptrty, ptr, val, align, vol, ordering, synchscope]
  42 -> label "FUNC_CODE_INST_STOREATOMIC_OLD" $ do
    notImplemented

  43 -> label "FUNC_CODE_INST_GEP" (parseGEP t Nothing r d)

  44 -> label "FUNC_CODE_INST_STORE" $ do
    let field = parseField r
    (ptr,ix)  <- getValueTypePair t r 0
    (val,ix') <- getValueTypePair t r ix

    Assert.recordSizeIn r [ix' + 2]

    aval      <- field ix' numeric
    let align | aval > 0  = Just (bit aval `shiftR` 1)
              | otherwise = Nothing
    effect (Store val ptr Nothing align) d

  -- LLVM 6: [ptrty, ptr, val, align, vol, ordering, ssid]
  45 -> label "FUNC_CODE_INST_STOREATOMIC" $ do
    (ptr, ix)  <- getValueTypePair t r 0
    (val, ix') <- getValueTypePair t r ix

    Assert.recordSizeIn r [ix' + 4]

    -- TODO: There's no spot in the AST for this ordering. Should there be?
    ordering <- getDecodedOrdering =<< parseField r (ix' + 2) unsigned
    when (ordering `elem` Nothing:map Just [Acquire, AcqRel]) $
      fail $ "Invalid atomic ordering: " ++ show ordering

    -- TODO: parse sync scope (ssid)

    -- copy-pasted from LOADATOMIC
    aval    <- parseField r ix' numeric
    let align | aval > 0  = Just (bit aval `shiftR` 1)
              | otherwise = Nothing

    effect (Store val ptr ordering align) d

  -- LLVM 6: [ptrty, ptr, cmp, new, vol, successordering, ssid,
  --          failureordering?, isweak?]
  46 -> label "FUNC_CODE_INST_CMPXCHG" $ do
    (ptr, ix)  <- getValueTypePair t r 0
    (val, ix') <- getValueTypePair t r ix
    new        <- getValue t (typedType val) =<< parseField r ix' numeric
    let ix''   = ix' + 1 -- TODO: is this right?

    -- TODO: record size assertion
    -- Assert.recordSizeGreater r (ix'' + 5)

    unless (typedType val `eqTypeModuloOpaquePtrs` typedType new)
      -- This test is generally validating the behavior of this library: the LLVM
      -- bitcode was likely generated by LLVM and is therefore "correct".
      $ fail $ unlines $
      [ "Mismatched value types:"
      , "cmp value: " ++ show (typedValue val)
      , "new value: " ++ show (typedValue new)
      , "cmp type:  " ++ show (typedType val)
      , "new type:  " ++ show (typedType new)
      ]

    volatile <- parseField r ix'' boolean

    successOrdering_ <- getDecodedOrdering =<< parseField r (ix'' + 1) unsigned
    let successOrderMsg ord = "Invalid success ordering: " ++ show ord
    successOrdering  <-
      case successOrdering_ of
        Nothing        -> fail (successOrderMsg successOrdering_)
        Just Unordered -> fail (successOrderMsg successOrdering_)
        Just ordering  -> pure ordering

    -- TODO: parse sync scope (ssid)
    -- ssid <- parseField r (ix'' + 2) ssid

    let len = length (recordFields r)
    failureOrdering_ <-
      if len < 7
      -- Implementation of getStrongestFailureOrdering in llvm/IR/Instructions.h:
      then case successOrdering of
             Release   -> pure (Just Monotonic)
             Monotonic -> pure (Just Monotonic)
             Acquire   -> pure (Just Acquire)   -- TODO: AcquireRelease?
             SeqCst    -> pure (Just SeqCst)
             _         -> fail (successOrderMsg successOrdering)
      else getDecodedOrdering =<< parseField r (ix'' + 3) unsigned
    failureOrdering  <-
      case failureOrdering_ of
        Nothing  -> fail "Invalid failure ordering (Nothing)"
        Just ord -> pure ord

    weak <-
      if len < 8
      then fail "Not yet implemented: old-style cmpxchg instruction (weak)"
      else parseField r (ix'' + 4) boolean

    -- The return type of cmpxchg is: the value that was present in the memory
    -- location and a boolean indicating whether it was overwritten.
    let ty = Struct [typedType val, PrimType (Integer 1)]
    result ty (CmpXchg weak volatile ptr val new Nothing successOrdering failureOrdering) d

  47 -> label "FUNC_CODE_LANDINGPAD" $ do
    Assert.recordSizeGreater r 2
    let field = parseField r
    ty         <- getType =<< field 0 numeric
    isCleanup  <- (/=(0::Int)) <$> field 1 numeric
    len        <- field 2 numeric
    clauses    <- parseClauses t r len 3
    result ty (LandingPad ty Nothing isCleanup clauses) d

  48 -> label "FUNC_CODE_CLEANUPRET" $ do
    -- Assert.recordSizeIn r [1, 2]
    notImplemented

  49 -> label "FUNC_CODE_CATCHRET" $ do
    -- Assert.recordSizeIn r [2]
    notImplemented

  50 -> label "FUNC_CODE_CATCHPAD" $ do
    notImplemented

  51 -> label "FUNC_CODE_CLEANUPPAD" $ do
    -- Assert.recordSizeGreater r [1]
    notImplemented

  52 -> label "FUNC_CODE_CATCHSWITCH" $ do
    -- Assert.recordSizeGreater r [1]
    notImplemented

  -- 53 is unused
  -- 54 is unused

  55 -> label "FUNC_CODE_OPERAND_BUNDLE" $ do
    notImplemented

  -- [opval,ty,opcode]
  56 -> label "FUNC_CODE_INST_UNOP" $ do
    let field = parseField r
    (v,ix)  <- getValueTypePair t r 0
    mkInstr <- field ix unop
    -- XXX: we're ignoring the fast-math flags
    result (typedType v) (mkInstr v) d

  -- LLVM 9: [attr, cc, norm, transfs, fnty, fnid, args...]
  57 -> label "FUNC_CODE_INST_CALLBR" $ do
    -- This implementation shares a lot with the parser for `call`, but they
    -- have different fields and so different handling of indices. In
    -- particular, the handling of basic block destinations is unique to
    -- `callbr`, and `callbr` doesn't support fast math flags.

    let field = parseField r
    -- pal <- field 0 numeric -- N.B. skipping param attributes
    ccinfo <- field 1 unsigned
    normal <- field 2 numeric
    numIndirect <- field 3 numeric
    indirectDests <- mapM (\idx -> field (4 + idx) numeric) [0..numIndirect - 1]
    let ix0 = 4 + numIndirect
    (mbFnTy, ix1) <- if testBit (ccinfo :: Word32) callExplicitTypeBit
                       then do fnTy <- getType =<< field ix0 numeric
                               return (Just fnTy, ix0+1)
                       else    return (Nothing,   ix0)

    (Typed opTy fn, ix2) <- getValueTypePair t r ix1
                                `mplus` fail "Invalid callbr record"

    fnty <- case mbFnTy of
             Just ty -> return ty
             Nothing -> do
               op <- Assert.elimPtrTo "callbr callee is not a pointer type" opTy
               case op of
                 FunTy{} -> return op
                 _       -> fail "Callee is not of pointer to function type"

    label (show fn) $ do
      (ret,as,va) <- elimFunTy fnty `mplus` fail "invalid CALLBR record"
      args <- parseCallArgs t va r ix2 as
      -- Use `fnty` instead of `opTy` as the function type, as `opTy` will be
      -- a pointer type. See Note [Typing function applications].
      result ret (CallBr fnty fn args normal indirectDests) d

  -- [opty, opval]
  58 -> label "FUNC_CODE_INST_FREEZE" $ do
    (v,_) <- getValueTypePair t r 0
    result (typedType v) (Freeze v) d

  -- LLVM 13: [ptrty, ptr, valty, val, operation, vol, ordering, ssid]
  59 -> label "FUNC_CODE_INST_ATOMICRMW" $
    parseAtomicRMW False t r d


  -- [opty,opval,opval,pred]
  code
   |  code == 9
   || code == 28 -> label "FUNC_CODE_INST_CMP2" $ do
    let field = parseField r
    (lhs,ix) <- getValueTypePair t r 0
               `mplus` (do i   <- adjustId =<< field 0 numeric
                           cxt <- getContext
                           return (forwardRef cxt i t, 1))
    rhs <- getValue t (typedType lhs) =<< field ix numeric

    let ty = typedType lhs
        parseOp | isPrimTypeOf isFloatingPoint ty ||
                  isVectorOf (isPrimTypeOf isFloatingPoint) ty =
                  return . FCmp <=< fcmpOp

                | otherwise =
                  return . ICmp <=< icmpOp

    op <- field (ix + 1) parseOp
    -- XXX: we're ignoring the fast-math flags

    let boolTy = Integer 1
    let rty = case ty of
          Vector n _ -> Vector n (PrimType boolTy)
          _          -> PrimType boolTy
    result rty (op lhs (typedValue rhs)) d

  -- unknown
   | otherwise -> fail ("instruction code " ++ show code ++ " is unknown")

parseFunctionBlockEntry _ _ d (valueSymtabBlockId -> Just _) = do
  -- this is parsed before any of the function block
  return d

parseFunctionBlockEntry globals t d (metadataBlockId -> Just es) = do
  (_, (globalUnnamedMds, localUnnamedMds), _, _, _) <- parseMetadataBlock globals t es
  if (null localUnnamedMds)
    then return d { partialGlobalMd = globalUnnamedMds <> partialGlobalMd d }
    else return d -- silently drop unexpected local unnamed metadata

parseFunctionBlockEntry globals t d (metadataAttachmentBlockId -> Just es) = do
  (_,(globalUnnamedMds, localUnnamedMds),instrAtt,fnAtt,_)
     <- parseMetadataBlock globals t es
  unless (null localUnnamedMds)
     (fail "parseFunctionBlockEntry PANIC: unexpected local unnamed metadata")
  unless (null globalUnnamedMds)
     (fail "parseFunctionBlockEntry PANIC: unexpected global unnamed metadata")
  return d { partialBody     = addInstrAttachments instrAtt (partialBody d)
           , partialMetadata = Map.union fnAtt (partialMetadata d)
           }

parseFunctionBlockEntry _ _ d (abbrevDef -> Just _) =
  -- ignore any abbreviation definitions
  return d

parseFunctionBlockEntry _ _ d (uselistBlockId -> Just _) = do
  -- ignore the uselist block
  return d

parseFunctionBlockEntry _ _ _ e = do
  fail ("function block: unexpected: " ++ show e)

addInstrAttachments :: InstrMdAttachments -> BlockList -> BlockList
addInstrAttachments atts blocks = go 0 (Map.toList atts) (Seq.viewl blocks)
  where
  go _   []  (b Seq.:< bs) = b Seq.<| bs
  go off mds (b Seq.:< bs) =
    b' Seq.<| go (off + numStmts) delay (Seq.viewl bs)
    where
    numStmts = Seq.length (partialStmts b)

    -- partition the attachments into those that apply to this block, and those
    -- that don't
    (use,delay)   = span applies mds
    applies (i,_) = i - off < numStmts

    b' | null use  = b
       | otherwise = b { partialStmts = foldl addMd (partialStmts b) use }

    addMd stmts (i,md') = Seq.adjust update (i-off) stmts
      where
      update (Result n s md) = Result n s (md ++ md')
      update (Effect   s md) = Effect   s (md ++ md')

  go _ _ Seq.EmptyL = Seq.empty

-- [n x operands]
parseGEP :: ValueTable -> Maybe Bool -> Record -> PartialDefine -> Parse PartialDefine
parseGEP t mbInBound r d = do
  (ib, ty, tv, r', ix) <-
      case mbInBound of

        -- FUNC_CODE_INST_GEP_OLD
        -- FUNC_CODE_INST_INBOUNDS_GEP_OLD
        Just ib -> do
          (tv,ix') <- getValueTypePair t r 0
          ty <- Assert.elimPtrTo "GEP not headed by pointer" (typedType tv)
          return (ib, ty, tv, r, ix')

        -- FUNC_CODE_INST_GEP
        Nothing -> do
          let r' = flattenRecord r
          let field = parseField r'
          ib <- field 0 boolean
          ty <- getType =<< field 1 numeric
          (tv,ix') <- getValueTypePair t r' 2
          return (ib, ty, tv, r', ix')

  args    <- label "parseGepArgs" (parseGepArgs t r' ix)
  rty     <- label "interpGep"    (interpGep ty tv args)
  result rty (GEP ib ty tv args) d

-- Parse an @atomicrmw@ instruction, which can be represented by one of the
-- following function codes:
--
-- * FUNC_CODE_INST_ATOMICRMW_OLD, which was introduced in LLVM 6.0.
--   [ptrty, ptr,        val, operation, vol, ordering, ssid]
--
-- * FUNC_CODE_INST_ATOMICRMW, which was introduced in LLVM 13.
--   [ptrty, ptr, valty, val, operation, vol, ordering, ssid]
--
-- The only difference between the two is that FUNC_CODE_INST_ATOMICRMW has a
-- @valty@ field, whereas FUNC_CODE_INST_ATOMICRMW_OLD does not (it reuses the
-- type that @ptrty@ points to as the @valty@).
--
-- The parsing code was inspired by the upstream code at
-- https://github.com/llvm/llvm-project/blob/2362c4ecdc88123e5d54c7ebe30889fbfa760a88/llvm/lib/Bitcode/Reader/BitcodeReader.cpp#L5658-L5720.
parseAtomicRMW :: Bool -> ValueTable -> Record -> PartialDefine -> Parse PartialDefine
parseAtomicRMW old t r d = do
  -- TODO: parse sync scope (ssid)
  (ptr, ix0) <- getValueTypePair t r 0

  (val, ix1) <-
    if old
      then -- FUNC_CODE_INST_ATOMICRMW_OLD
           do ty <- Assert.elimPtrTo "atomicrmw instruction not headed by pointer" (typedType ptr)
              typed <- getValue t ty =<< parseField r ix0 numeric
              if ty /= (typedType typed)
              then fail $ unlines $ [ "Wrong type of value retrieved from value table"
                                    , "Expected: " ++ show (ty)
                                    , "Got: " ++ show (typedType typed)
                                    ]
              else pure (typed, ix0 + 1)
      else -- FUNC_CODE_INST_ATOMICRMW
           getValueTypePair t r ix0

  -- Catch incorrect operand types
  let valTy = typedType val
  unless (case valTy of
            PrimType (Integer   _) -> True
            PrimType (FloatType _) -> True
            _                      -> False) $
    fail $ "Expected integer or float operand, found " ++ show valTy

  -- TODO: enable this assertion. Is getTypeValuePair returning the wrong value?
  -- when (length (recordFields r) /= ix1 + 4) $ do
  --   fail $ "Invalid record size: " ++ show (length (recordFields r))

  operation <- getDecodedAtomicRWOp =<< parseField r ix1 numeric
  volatile  <- parseField r (ix1 + 1) nonzero
  ordering  <- parseField r (ix1 + 2) unsigned >>= getDecodedOrdering >>=
    \case
      Just ordering -> pure ordering
      Nothing       -> fail $ "`atomicrmw` requires ordering: ix == " ++ show ix1

  result (typedType val) (AtomicRW volatile operation ptr val Nothing ordering) d

-- | Generate a statement that doesn't produce a result.
effect :: Instr' Int -> PartialDefine -> Parse PartialDefine
effect i d = addStmt (Effect i []) d

-- | Try to name results, fall back on leaving them as effects.
result :: Type -> Instr' Int -> PartialDefine -> Parse PartialDefine
result (PrimType Void) i d = effect i d
result ty              i d = do
  res <- nameNextValue ty
  addStmt (Result res i []) d

-- | Loop, parsing arguments out of a record in pairs, as the arguments to a phi
-- instruction.
parsePhiArgs :: Bool -> ValueTable -> Record -> Parse [(PValue,Int)]
parsePhiArgs relIds t r = loop 1
  where
  field = parseField r
  len   = length (recordFields r)

  getId n
    | relIds    = do
      i   <- field n signedWord64
      pos <- getNextId
      return (pos - fromIntegral i)
    | otherwise =
      field n numeric

  parse n = do
    i   <- getId n
    cxt <- getContext
    let val = forwardRef cxt i t
    bid <- field (n+1) numeric
    return (typedValue val,bid)

  loop n
    -- We must stop when @n@ is either equal to @len@, or to @len - 1@ in the
    -- presence of fast math flags.
    | len - n < 2 = return []
    | otherwise   = (:) <$> parse n <*> loop (n + 2)

-- | Parse the arguments for a call record.
parseCallArgs :: ValueTable -> Bool -> Record -> Int -> [Type] -> Parse [Typed PValue]
parseCallArgs t b r = parseArgs t op b r
 where
 op ty i =
  case ty of
    PrimType Label -> return (Typed ty (ValLabel i))
    _              -> getValue t ty i

-- | Parse the arguments for an invoke record.
parseInvokeArgs :: ValueTable -> Bool -> Record -> Int -> [Type] -> Parse [Typed PValue]
parseInvokeArgs t = parseArgs t (getValue t)

-- | Parse arguments for the invoke and call instructions.
parseArgs :: ValueTable -> (Type -> Int -> Parse (Typed PValue))
          -> Bool -> Record -> Int -> [Type] -> Parse [Typed PValue]
parseArgs t parse va r = loop
  where
  field = parseField r

  len = length (recordFields r)

  loop ix (ty:tys) = do
    tv   <- parse ty =<< field ix numeric
    rest <- loop (ix+1) tys
    return (tv:rest)
  loop ix []
    | va        = varArgs ix
    | otherwise = return []

  varArgs ix
    | ix < len  = do
      (tv,ix') <- getValueTypePair t r ix
      rest     <- varArgs ix'
      return (tv:rest)
    | otherwise = return []


parseGepArgs :: ValueTable -> Record -> Int -> Parse [Typed PValue]
parseGepArgs t r = loop
  where
  loop n = parse `mplus` return []
    where
    parse = do
      (tv,ix') <- getValueTypePair t r n
      rest     <- loop ix'
      return (tv:rest)

-- | Interpret a getelementptr instruction to determine its result type.
interpGep :: Type -> Typed PValue -> [Typed PValue] -> Parse Type
interpGep baseTy ptr vs = check (resolveGep baseTy ptr vs)
  where
  check res = case res of
    HasType rty -> return (PtrTo rty)
    Invalid -> fail $ unlines $
      [ "Unable to determine the type of getelementptr"
      , "Base type: " ++ show baseTy
      , "Pointer value: " ++ show ptr
      ]
    Resolve i k -> do
      ty' <- getType' =<< getTypeId i
      check (k ty')

parseIndexes :: (Num a, Bits a) => Record -> Int -> Parse [a]
parseIndexes r = loop
  where
  field  = parseField r
  loop n = do
    ix   <- field n numeric
    rest <- loop (n+1) `mplus` return []
    return (ix:rest)

interpValueIndex :: Type    -- ^ Aggregate (struct/array) type to index into
                 -> [Int32] -- ^ Indices
                 -> Parse Type
interpValueIndex ty is = check (resolveValueIndex ty is)
  where
  check res = case res of
    Invalid ->
      fail $ unlines $
        [ "Unable to determine the return type of `extractvalue`"
        , "Hint: The input type should be an aggregate type (struct or array)"
        , "Input type: " ++ show ty
        , "Indices: " ++ show is
        ]
    HasType rty -> return rty
    Resolve i k -> do
      ty' <- getType' =<< getTypeId i
      check (k ty')

-- | Parse out the integer values, and jump targets (as Int labels) for a switch
-- instruction.  For example, parsing the following switch instruction
--
-- >  switch i32 %Val, label %truedest [i32 0, label %falsedest]
--
-- yields the list [0,Ident "falsedest"], if labels are just 'Ident's.
parseSwitchLabels :: Type -> Record -> Int -> Parse [(Integer,Int)]
parseSwitchLabels ty r = loop
  where
  field = parseField r
  len   = length (recordFields r)

  loop n
    | n >= len  = return []
    | otherwise = do
      tv <- getFnValueById ty =<< field n numeric
      i <- case typedValue tv of
             ValInteger i -> return i
             ValBool b -> return (toEnum (fromEnum b))
             v -> fail $ unwords [ "Invalid value in SWITCH record. Found"
                                 , show v
                                 , "at position"
                                 , show n
                                 ]
      l    <- field (n+1) numeric
      rest <- loop (n+2)
      return ((i,l):rest)

-- | See the comment for 'parseSwitchLabels' for information about what this
-- does.
parseNewSwitchLabels :: Word32 -> Record -> Int -> Int -> Parse [(Integer,Int)]
parseNewSwitchLabels width r = loop
  where
  field = parseField r
  len   = length (recordFields r)

  -- parse each group of cases as one or more numbers, and a basic block.
  loop numCases n
    | numCases <= 0 = return []
    | n >= len      = fail "invalid SWITCH record"
    | otherwise     = do
      numItems <- field n  numeric
      (ls,n')  <- parseItems numItems (n + 1)
      lab      <- field n' numeric
      rest     <- loop (numCases - 1) (n' + 1)
      return ([ (l,lab) | l <- ls ] ++ rest)

  -- different numbers that all target the same basic block
  parseItems :: Int -> Int -> Parse ([Integer],Int)
  parseItems numItems n
    | numItems <= 0 = return ([],n)
    | otherwise     = do
      isSingleNumber <- field n boolean

      -- The number of words used to represent a case is only specified when the
      -- value comes from a large type.
      (activeWords,lowStart) <-
        if width > 64
           then do aw <- field (n + 1) numeric
                   return (aw, n + 2)
           else return (1,n+1)

      -- read the chunks of the number in.  each chunk represents one 64-bit
      -- limb of a big num.
      chunks <- parseSlice r lowStart activeWords signedWord64

      -- decode limbs in big-endian order
      let low = foldr (\l acc -> acc `shiftL` 64 + toInteger l) 0 chunks

      (num,n') <-
        if isSingleNumber
           then return (low, lowStart + activeWords)
           else fail "Unhandled case in switch: Please send in this test case!"

      (rest,nFinal) <- parseItems (numItems - 1) n'

      return (num:rest,nFinal)




type PClause = Clause' Int

parseClauses :: ValueTable -> Record -> Int -> Int -> Parse [PClause]
parseClauses t r = loop
  where
  loop n ix
    | n <= 0    = return []
    | otherwise = do
      cty       <- parseField r ix numeric
      (val,ix') <- getValueTypePair t r (ix + 1)
      cs        <- loop (n-1) ix'
      case cty :: Int of
        0 -> return (Catch  val : cs)
        1 -> return (Filter val : cs)
        _ -> fail ("Invalid clause type: " ++ show cty)


-- | 'Nothing' corresponds to @NotAtomic@ in the LLVM source
getDecodedOrdering :: Word32 -> Parse (Maybe AtomicOrdering)
getDecodedOrdering 0 = return Nothing
getDecodedOrdering 1 = return (Just Unordered)
getDecodedOrdering 2 = return (Just Monotonic)
getDecodedOrdering 3 = return (Just Acquire)
getDecodedOrdering 4 = return (Just Release)
getDecodedOrdering 5 = return (Just AcqRel)
getDecodedOrdering 6 = return (Just SeqCst)
getDecodedOrdering i = Assert.unknownEntity "atomic ordering" i


-- https://github.com/llvm/llvm-project/blob/e24dc34aa085b9e8d3ea58cc5f59f80bc4c7cdb4/llvm/include/llvm/Bitcode/LLVMBitCodes.h#L468-L489
getDecodedAtomicRWOp :: Integer -> Parse AtomicRWOp
getDecodedAtomicRWOp 0  = pure AtomicXchg
getDecodedAtomicRWOp 1  = pure AtomicAdd
getDecodedAtomicRWOp 2  = pure AtomicSub
getDecodedAtomicRWOp 3  = pure AtomicAnd
getDecodedAtomicRWOp 4  = pure AtomicNand
getDecodedAtomicRWOp 5  = pure AtomicOr
getDecodedAtomicRWOp 6  = pure AtomicXor
getDecodedAtomicRWOp 7  = pure AtomicMax
getDecodedAtomicRWOp 8  = pure AtomicMin
getDecodedAtomicRWOp 9  = pure AtomicUMax
getDecodedAtomicRWOp 10 = pure AtomicUMin
getDecodedAtomicRWOp 11 = pure AtomicFAdd
getDecodedAtomicRWOp 12 = pure AtomicFSub
getDecodedAtomicRWOp 13 = pure AtomicFMax
getDecodedAtomicRWOp 14 = pure AtomicFMin
getDecodedAtomicRWOp 15 = pure AtomicUIncWrap
getDecodedAtomicRWOp 16 = pure AtomicUDecWrap
getDecodedAtomicRWOp v  = Assert.unknownEntity "atomic RWOp" v

{-
Note [Typing function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The LLVM Language Reference Manual says the following about the `call`
instruction:

    <result> = [tail | musttail | notail ] call [fast-math flags] [cconv] [ret attrs] [addrspace(<num>)]
               <ty>|<fnty> <fnptrval>(<function args>) [fn attrs] [ operand bundles ]

    ...

    * ‘fnty’: shall be the signature of the function being called. [...]
    * ‘fnptrval’: An LLVM value containing a pointer to a function to be called. [...]

One slightly surprising aspect of this instruction (at least, this was not
obvious to me when I first read it) is that `fnptrval` does _not_ have the type
`fnty`. Rather, `fnptrval` has a pointer type, and the underlying memory being
pointed to is treated as having type `fnty`.

A consequence of this property is that `fnty` is /never/ a pointer type. It is
easy to accidentally give the `fnty` of a `call` instruction a pointer type if
you simply compute the type of `fnptrval`, but this is incorrect. This is
especially incorrect in a world where pointers are opaque (see
https://llvm.org/docs/OpaquePointers.html), as you cannot know know what a
pointer's pointee type is by looking at the type alone. Instead, you must look
at the surrounding instruction to know what the pointee type is. In the case of
the `call` instruction, the pointee type for `fnptrval` is precisely `fnty`.

Similar reasoning applies to the `callbr` and `invoke` instructions, which also
invoke a pointer whose underlying memory has a function type.
-}
