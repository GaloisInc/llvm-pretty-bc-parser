{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.LLVM.BitCode.IR.Constants where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.IR.Values
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST

import Control.Monad (mplus,mzero,foldM,(<=<),when)
import Control.Monad.ST (runST,ST)
import Data.Array.ST (newArray,readArray,MArray,STUArray)
import Data.Bits (shiftL,shiftR,testBit)
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Word (Word32,Word64)
import qualified Data.Map as Map

#if __GLASGOW_HASKELL__ >= 704
import Data.Array.Unsafe (castSTUArray)
#else
import Data.Array.ST (castSTUArray)
#endif


-- Instruction Field Parsing ---------------------------------------------------

-- | Parse a binop from a field, returning its constructor in the AST.
binopGeneric :: forall a.
                (ArithOp -> Typed PValue -> PValue -> a)
             -> (BitOp -> Typed PValue -> PValue -> a)
             -> Match Field (Maybe Int -> Typed PValue -> PValue -> a)
binopGeneric aop bop = choose <=< numeric
  where

  constant k kf = return $ \_mb x y ->
    case typedType x of
      PrimType (FloatType _) -> kf x y
      _ -> k x y

  nuw x = testBit x 0
  nsw x = testBit x 1

  -- operations that accept the nuw and nsw flags
  wrapFlags i k kf = return $ \ mb x y ->
    case typedType x of
      PrimType (FloatType _) -> i kf x y
      _ ->
        case mb of
          Nothing -> i (k  False   False)  x y
          Just w  -> i (k (nuw w) (nsw w)) x y

  exact x = testBit x 0

  -- operations that accept the exact flag
  exactFlag i k kf = return $ \ mb x y ->
    case typedType x of
      PrimType (FloatType _) -> i kf x y
      _ ->
        case mb of
          Nothing -> i (k  False)    x y
          Just w  -> i (k (exact w)) x y

  choose :: Match Int (Maybe Int -> Typed PValue -> PValue -> a)
  choose 0  = wrapFlags aop Add   FAdd
  choose 1  = wrapFlags aop Sub   FSub
  choose 2  = wrapFlags aop Mul   FMul
  choose 3  = exactFlag aop UDiv  FDiv
  choose 4  = exactFlag aop SDiv  FDiv
  choose 5  = constant (aop URem) (aop FRem)
  choose 6  = constant (aop SRem) (aop FRem)
  choose 7  = wrapFlags bop Shl  (error "invalid shl on floating point")
  choose 8  = exactFlag bop Lshr (error "invalid lshr on floating point")
  choose 9  = exactFlag bop Ashr (error "invalid ashr on floating point")
  choose 10 = constant (bop And) (error "invalid and on floating point")
  choose 11 = constant (bop Or)  (error "invalid or on floating point")
  choose 12 = constant (bop Xor) (error "invalid xor on floating point")
  choose _  = mzero

binop :: Match Field (Maybe Int -> Typed PValue -> PValue -> PInstr)
binop = binopGeneric Arith Bit

binopCE :: Match Field (Maybe Int -> Typed PValue -> PValue -> PValue)
binopCE = binopGeneric aop bop
  where
  aop op tv v = ValConstExpr (ConstArith op tv v)
  bop op tv v = ValConstExpr (ConstBit op tv v)

fcmpOp :: Match Field FCmpOp
fcmpOp  = choose <=< numeric
  where
  choose :: Match Int FCmpOp
  choose 0  = return Ffalse
  choose 1  = return Foeq
  choose 2  = return Fogt
  choose 3  = return Foge
  choose 4  = return Folt
  choose 5  = return Fole
  choose 6  = return Fone
  choose 7  = return Ford
  choose 8  = return Funo
  choose 9  = return Fueq
  choose 10 = return Fugt
  choose 11 = return Fuge
  choose 12 = return Fult
  choose 13 = return Fule
  choose 14 = return Fune
  choose 15 = return Ftrue
  choose _  = mzero

icmpOp :: Match Field ICmpOp
icmpOp  = choose <=< numeric
  where
  choose :: Match Int ICmpOp
  choose 32 = return Ieq
  choose 33 = return Ine
  choose 34 = return Iugt
  choose 35 = return Iuge
  choose 36 = return Iult
  choose 37 = return Iule
  choose 38 = return Isgt
  choose 39 = return Isge
  choose 40 = return Islt
  choose 41 = return Isle
  choose _  = mzero

castOpGeneric :: forall c. (ConvOp -> Maybe c) -> Match Field c
castOpGeneric op = choose <=< numeric
  where
  choose :: Match Int c
  choose 0  = op Trunc
  choose 1  = op ZExt
  choose 2  = op SExt
  choose 3  = op FpToUi
  choose 4  = op FpToSi
  choose 5  = op UiToFp
  choose 6  = op SiToFp
  choose 7  = op FpTrunc
  choose 8  = op FpExt
  choose 9  = op PtrToInt
  choose 10 = op IntToPtr
  choose 11 = op BitCast
  choose _  = mzero

castOp :: Match Field (Typed PValue -> Type -> PInstr)
castOp = castOpGeneric (return . Conv)

castOpCE :: Match Field (Typed PValue -> Type -> PValue)
castOpCE = castOpGeneric op
  where
  op c = return (\ tv t -> ValConstExpr (ConstConv c tv t))

-- Constants Block -------------------------------------------------------------

type ConstantTable = Map.Map Int (Typed Value)

cstGep :: Match Entry Record
cstGep  = hasRecordCode 12 <=< fromEntry

cstInboundsGep :: Match Entry Record
cstInboundsGep  = hasRecordCode 20 <=< fromEntry

setCurType :: Int -> Parse Type
setCurType  = getType'


-- Constants Block Parsing -----------------------------------------------------

-- | Parse the entries of the constants block.
parseConstantsBlock :: [Entry] -> Parse ()
parseConstantsBlock es = fixValueTable_ $ \ vs' -> do
  let curTy = fail "no current type id set"
  (_,vs) <- foldM (parseConstantEntry vs') (curTy,[]) es
  return vs

-- | Parse entries of the constant table.
parseConstantEntry :: ValueTable -> (Parse Type,[Typed PValue]) -> Entry
                   -> Parse (Parse Type, [Typed PValue])

parseConstantEntry t (getTy,cs) (fromEntry -> Just r) =
 label "CONSTANTS_BLOCK" $ case recordCode r of

  1 -> label "CST_CODE_SETTYPE" $ do
    let field = parseField r
    i <- field 0 numeric
    return (setCurType i, cs)

  2 -> label "CST_CODE_NULL" $ do
    ty  <- getTy
    val <- resolveNull ty
    return (getTy, Typed ty val:cs)

  3 -> label "CST_CODE_UNDEF" $ do
    ty <- getTy
    return (getTy, Typed ty ValUndef:cs)

  -- [intval]
  4 -> label "CST_CODE_INTEGER" $ do
    let field = parseField r
    ty <- getTy
    n  <- field 0 signedWord64
    let val = fromMaybe (ValInteger (toInteger n)) $ do
                Integer 0 <- elimPrimType ty
                return (ValBool (n /= 0))
    return (getTy, Typed ty val:cs)

  -- [n x value]
  5 -> label "CST_CODE_WIDE_INTEGER" $ do
    ty <- getTy
    n  <- parseWideInteger r
    return (getTy, Typed ty (ValInteger n):cs)

  -- [fpval]
  6 -> label "CST_CODE_FLOAT" $ do
    let field = parseField r
    ty <- getTy
    ft <- (elimFloatType =<< elimPrimType ty)
        `mplus` fail "expecting a float type"
    let build k = do
          w <- field 0 numeric
          return (getTy, (Typed ty $! k w):cs)
    case ft of
      Float -> build (ValFloat  . castFloat)
      _     -> build (ValDouble . castDouble)

  -- [n x value number]
  7 -> label "CST_CODE_AGGREGATE" $ do
    ty    <- getTy
    elems <- parseField r 0 (fieldArray numeric)
        `mplus` parseFields r 0 numeric
    cxt <- getContext
    let vals = [forwardRef cxt ix t | ix <- elems ]
    case ty of

      Struct _fs ->
        return (getTy, Typed ty (ValStruct vals):cs)

      PackedStruct _fs ->
        return (getTy, Typed ty (ValPackedStruct vals):cs)

      Array _n fty ->
        return (getTy, Typed ty (ValArray fty (map typedValue vals)):cs)

      Vector _n ety -> do
        return (getTy, Typed ty (ValVector ety (map typedValue vals)):cs)

      _ -> return (getTy, Typed ty ValUndef:cs)

  -- [values]
  8 -> label "CST_CODE_STRING" $ do
    let field = parseField r
    ty     <- getTy
    values <- field 0 string
    return (getTy, Typed ty (ValString values):cs)

  -- [values]
  9 -> label "CST_CODE_CSTRING" $ do
    ty     <- getTy
    values <- parseField r 0 cstring
        `mplus` parseFields r 0 (fieldChar6 ||| char)
    return (getTy, Typed ty (ValString (values ++ [chr 0])):cs)

  -- [opcode,opval,opval]
  10 -> label "CST_CODE_CE_BINOP" $ do
    let field = parseField r
    ty      <- getTy
    mkInstr <- field 0 binopCE
    lopval  <- field 1 numeric
    ropval  <- field 2 numeric
    cxt     <- getContext
    let lv = forwardRef cxt lopval t
        rv = forwardRef cxt ropval t
    let mbWord = numeric =<< fieldAt 3 r
    return (getTy, Typed ty (mkInstr mbWord lv (typedValue rv)) : cs)

  -- [opcode, opty, opval]
  11 -> label "CST_CODE_CE_CAST" $ do
    let field = parseField r
    ty   <- getTy
    -- We're not handling the opcode < 0 case here, in which the cast is
    -- reported as ``unknown.''
    cast'  <- field 0 castOpCE
    opval  <- field 2 numeric
    cxt    <- getContext
    return (getTy,Typed ty (cast' (forwardRef cxt opval t) ty):cs)

  -- [n x operands]
  12 -> label "CST_CODE_CE_GEP" $ do
    ty <- getTy
    v <- parseCeGep False t r
    return (getTy,Typed ty v:cs)

  -- [opval,opval,opval]
  13 -> label "CST_CODE_CE_SELECT" $ do
    let field = parseField r
    ty  <- getTy
    ix1 <- field 0 numeric
    ix2 <- field 1 numeric
    ix3 <- field 2 numeric
    cxt <- getContext
    let ref ix = forwardRef cxt ix t
        ce     = ConstSelect (ref ix1) (ref ix2) (ref ix3)
    return (getTy, Typed ty (ValConstExpr ce):cs)

  -- [opty,opval,opval]
  14 -> label "CST_CODE_CE_EXTRACTELT" $ do
    notImplemented

  15 -> label "CST_CODE_CE_INSERTELT" $ do
    notImplemented

  16 -> label "CST_CODE_CE_SHUFFLEVEC" $ do
    notImplemented

  -- [opty, opval, opval, pred]
  17 -> label "CST_CODE_CE_CMP" $ do
    let field = parseField r
    opty <- getType                  =<< field 0 numeric
    op0  <- getConstantFwdRef t opty =<< field 1 numeric
    op1  <- getConstantFwdRef t opty =<< field 2 numeric

    let isFloat = isPrimTypeOf isFloatingPoint
    cst <- if isFloat opty || isVectorOf isFloat opty
              then do op <- field 3 fcmpOp
                      return (ConstFCmp op op0 op1)

              else do op <- field 3 icmpOp
                      return (ConstICmp op op0 op1)

    return (getTy, Typed (PrimType (Integer 1)) (ValConstExpr cst):cs)

  18 -> label "CST_CODE_INLINEASM_OLD" $ do
    let field = parseField r
    ty    <- getTy
    flags <- field 0 numeric
    let sideEffect = testBit (flags :: Int) 0
        alignStack = (flags `shiftR` 1) == 1

    alen <- field 1 numeric
    asm  <- parseSlice r 2 alen char

    clen <- field (2+alen) numeric
    cst  <- parseSlice r (3+alen) clen char

    return (getTy, Typed ty (ValAsm sideEffect alignStack asm cst):cs)

  19 -> label "CST_CODE_CE_SHUFFLEVEC_EX" $ do
    notImplemented

  -- [n x operands]
  20 -> label "CST_CODE_CE_INBOUNDS_GEP" $ do
    ty <- getTy
    v <- parseCeGep True t r
    return (getTy,Typed ty v:cs)

  -- [funty,fnval,bb#]
  21 -> label "CST_CODE_BLOCKADDRESS" $ do
    let field = parseField r
    ty  <- getTy
    val <- getValue ty =<< field 1 numeric
    bid <-                 field 2 numeric
    sym <- elimValSymbol (typedValue val)
        `mplus` fail "invalid function symbol in BLOCKADDRESS record"
    let ce = ConstBlockAddr sym bid
    return (getTy, Typed ty (ValConstExpr ce):cs)

  -- [n x elements]
  22 -> label "CST_CODE_DATA" $ do
    ty     <- getTy
    elemTy <- (elimPrimType =<< elimSequentialType ty)
        `mplus` fail "invalid container type for CST_CODE_DATA"
    let build mk = do
          ns <- parseFields r 0 numeric
          let elems            = map mk ns
              val | isArray ty = ValArray (PrimType elemTy) elems
                  | otherwise  = ValVector (PrimType elemTy) elems
          return (getTy, Typed ty val : cs)
    case elemTy of
      Integer 8        -> build ValInteger
      Integer 16       -> build ValInteger
      Integer 32       -> build ValInteger
      Integer 64       -> build ValInteger
      FloatType Float  -> build ValFloat
      FloatType Double -> build ValDouble
      _                -> fail "unknown element type in CE_DATA"

  23 -> label "CST_CODE_INLINEASM" $ do
    let field = parseField r
    mask <- field 0 numeric

    let test = testBit (mask :: Word32)
        hasSideEffects = test 0
        isAlignStack   = test 1
        _asmDialect    = mask `shiftR` 2

    let len = length (recordFields r)
    asmStrSize <- field 1 numeric
    when (2 + asmStrSize >= len)
         (fail "Invalid record")

    constStrSize <- field (2 + asmStrSize) numeric
    when (3 + asmStrSize + constStrSize > len)
         (fail "Invalid record")

    asmStr   <- parseSlice r  2               asmStrSize   char
    constStr <- parseSlice r (3 + asmStrSize) constStrSize char

    ty <- getTy
    let val = ValAsm hasSideEffects isAlignStack asmStr constStr

    return (getTy, Typed ty val : cs)




  code -> fail ("unknown constant record code: " ++ show code)

parseConstantEntry _ st (abbrevDef -> Just _) =
  -- ignore abbreviation definitions
  return st

parseConstantEntry _ _ e =
  fail ("constant block: unexpected: " ++ show e)

parseCeGep :: Bool -> ValueTable -> Record -> Parse PValue
parseCeGep isInbounds t r = do
  let isExplicit = odd (length (recordFields r))
      firstIdx = if isExplicit then 1 else 0
      field = parseField r
      loop n = do
        ty   <- getType =<< field  n    numeric
        elt  <-             field (n+1) numeric
        rest <- loop (n+2) `mplus` return []
        cxt  <- getContext
        return (Typed ty (typedValue (forwardRef cxt elt t)) : rest)
  mPointeeType <-
    if isExplicit
    then Just <$> (getType =<< field 0 numeric)
    else pure Nothing
  args <- loop firstIdx
  return $! ValConstExpr (ConstGEP isInbounds mPointeeType args)

parseWideInteger :: Record -> Parse Integer
parseWideInteger r = do
  limbs <- parseFields r 0 signedWord64
  return (foldr (\l acc -> acc `shiftL` 64 + (toInteger l)) 0 limbs)

resolveNull :: Type -> Parse PValue
resolveNull ty = case typeNull ty of
  HasNull nv    -> return nv
  ResolveNull i -> resolveNull =<< getType' =<< getTypeId i


-- Float/Double Casting --------------------------------------------------------

castFloat :: Word32 -> Float
castFloat w = runST (cast w)

castDouble :: Word64 -> Double
castDouble w = runST (cast w)

cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s))
     => a -> ST s b
cast x = do
  arr <- newArray (0 :: Int, 0) x
  res <- castSTUArray arr
  readArray res 0
