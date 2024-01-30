{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Constants where

import qualified Data.LLVM.BitCode.Assert as Assert
import           Data.LLVM.BitCode.Bitstream
import           Data.LLVM.BitCode.Match
import           Data.LLVM.BitCode.Parse
import           Data.LLVM.BitCode.Record
import           Text.LLVM.AST

import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import           Control.Monad (mplus,mzero,foldM,(<=<), when)
import           Control.Monad.ST (runST,ST)
import           Data.Array.ST (newArray,readArray,MArray,STUArray)
import           Data.Bits (shiftL,shiftR,testBit, Bits)
import           Data.LLVM.BitCode.BitString ( pattern Bits' )
import qualified Data.LLVM.BitCode.BitString as BitS
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word16, Word32,Word64)

#if __GLASGOW_HASKELL__ >= 704
import           Data.Array.Unsafe (castSTUArray)
#else
import           Data.Array.ST (castSTUArray)
#endif

import           Prelude


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

unopGeneric :: forall a.
               (UnaryArithOp -> Typed PValue -> a)
            -> Match Field (Typed PValue -> a)
unopGeneric uaop = choose <=< numeric
  where
  choose :: Match Int (Typed PValue -> a)
  choose 0 = return (uaop FNeg)
  choose _ = mzero

unop :: Match Field (Typed PValue -> PInstr)
unop = unopGeneric UnaryArith

unopCE :: Match Field (Typed PValue -> PValue)
unopCE = unopGeneric uaop
  where
  uaop op tv = ValConstExpr (ConstUnaryArith op tv)

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
    n  <- parseWideInteger r 0
    return (getTy, Typed ty (ValInteger n):cs)

  -- [fpval]
  6 -> label "CST_CODE_FLOAT" $ do
    ty <- getTy
    ft <- (elimFloatType =<< elimPrimType ty)
        `mplus` fail "expecting a float type"
    let build :: (Num a, Bits a) => (a -> PValue) -> Parse (Parse Type, [Typed PValue])
        build k = do
          a <-  parseField r 0 (fmap k . numeric)
          return (getTy, (Typed ty $! a):cs)
    case ft of
      Float -> build (ValFloat  . castFloat)
      Double -> build (ValDouble . castDouble)
      X86_fp80 -> fp80build ty r cs getTy
      _ -> error $ "parseConstantEntry: Unsupported type " ++ show ft

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
    values <- field 0 (fieldArray char)
    return (getTy, Typed ty (ValString values):cs)

  -- [values]
  9 -> label "CST_CODE_CSTRING" $ do
    ty     <- getTy
    values <- parseField r 0 (fieldArray (fieldChar6 ||| char))
        `mplus` parseFields r 0 (fieldChar6 ||| char)
    return (getTy, Typed ty (ValString (values ++ [0])):cs)

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
    v <- parseCeGep CeGepCode12 t r
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
    opty <- getType =<< field 0 numeric
    ix0  <- field 1 numeric
    ix1  <- field 2 numeric
    cxt  <- getContext
    let op0 = forwardRef cxt ix0 t
    let op1 = forwardRef cxt ix1 t

    let isFloat = isPrimTypeOf isFloatingPoint
    cst <- if isFloat opty || isVectorOf isFloat opty
              then do op <- field 3 fcmpOp
                      return (ConstFCmp op op0 op1)

              else do op <- field 3 icmpOp
                      return (ConstICmp op op0 op1)

    return (getTy, Typed (PrimType (Integer 1)) (ValConstExpr cst):cs)

  18 -> label "CST_CODE_INLINEASM_OLD" $ do
    tv <- parseInlineAsm InlineAsmCode18 getTy r
    return (getTy, tv:cs)

  19 -> label "CST_CODE_CE_SHUFFLEVEC_EX" $ do
    notImplemented

  -- [n x operands]
  20 -> label "CST_CODE_CE_INBOUNDS_GEP" $ do
    ty <- getTy
    v <- parseCeGep CeGepCode20 t r
    return (getTy,Typed ty v:cs)

  -- [funty,fnval,bb#]
  21 -> label "CST_CODE_BLOCKADDRESS" $ do
    when (length (recordFields r) < 3) $
      fail "Invalid BLOCKADDRESS record (length < 3)"
    let field = parseField r
    ty  <- getTy
    ctx <- getContext
    valref <- field 1 numeric
    bid <- field 2 numeric
    let ce = ConstBlockAddr (forwardRef ctx valref t) bid
    return (getTy, Typed ty (ValConstExpr ce) : cs)

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
      Integer 8          -> build ValInteger
      Integer 16         -> build ValInteger
      Integer 32         -> build ValInteger
      Integer 64         -> build ValInteger
      FloatType Float    -> build (ValFloat . castFloat)
      FloatType Double   -> build (ValDouble . castDouble)
      x                  -> Assert.unknownEntity "element type" x

  23 -> label "CST_CODE_INLINEASM_OLD2" $ do
    tv <- parseInlineAsm InlineAsmCode23 getTy r
    return (getTy, tv:cs)

  -- [opty, flags, n x operands]
  24 -> label "CST_CODE_CE_GEP_WITH_INRANGE_INDEX" $ do
    ty <- getTy
    v <- parseCeGep CeGepCode24 t r
    return (getTy,Typed ty v:cs)

  -- [opcode, opval]
  25 -> label "CST_CODE_CE_UNOP" $ do
    let field = parseField r
    ty      <- getTy
    mkInstr <- field 0 unopCE
    opval   <- field 1 numeric
    cxt     <- getContext
    let v = forwardRef cxt opval t
    return (getTy, Typed ty (mkInstr v) : cs)

  26 -> label "CST_CODE_POISON" $ do
    ty <- getTy
    return (getTy, Typed ty ValPoison : cs)

  27 -> label "CST_CODE_DSO_LOCAL_EQUIVALENT" $ do
    notImplemented

  28 -> label "CST_CODE_INLINEASM_OLD3" $ do
    tv <- parseInlineAsm InlineAsmCode28 getTy r
    return (getTy, tv:cs)

  29 -> label "CST_CODE_NO_CFI_VALUE" $ do
    notImplemented

  30 -> label "CST_CODE_INLINEASM" $ do
    tv <- parseInlineAsm InlineAsmCode30 getTy r
    return (getTy, tv:cs)

  code -> Assert.unknownEntity "constant record code" code

parseConstantEntry _ st (abbrevDef -> Just _) =
  -- ignore abbreviation definitions
  return st

parseConstantEntry _ _ e =
  fail ("constant block: unexpected: " ++ show e)

-- | The different codes for constant @getelementptr@ expressions. Each one has
-- minor differences in how they are parsed.
data CeGepCode
  = CeGepCode12
  -- ^ @CST_CODE_CE_GEP = 12@. The original.
  | CeGepCode20
  -- ^ @CST_CODE_CE_INBOUNDS_GEP = 20@. This adds an @inbounds@ field that
  -- indicates that the result value should be poison if it performs an
  -- out-of-bounds index.
  | CeGepCode24
  -- ^ @CST_CODE_CE_GEP_WITH_INRANGE_INDEX = 24@. This adds an @inrange@ field
  -- that indicates that loading or storing to the result pointer will have
  -- undefined behavior if the load or store would access memory outside of the
  -- bounds of the indices marked as @inrange@.
  deriving Eq

-- | Parse a 'ConstGEP' value. There are several variations on this theme that
-- are captured in the 'CeGepCode' argument.
parseCeGep :: CeGepCode -> ValueTable -> Record -> Parse PValue
parseCeGep code t r = do
  let field = parseField r

  (mbBaseTy, ix0) <-
    if code == CeGepCode24 || odd (length (recordFields r))
    then do baseTy <- getType =<< field 0 numeric
            pure (Just baseTy, 1)
    else pure (Nothing, 0)

  (isInbounds, mInrangeIdx, ix1) <-
    case code of
      CeGepCode12 -> pure (False, Nothing, ix0)
      CeGepCode20 -> pure (True, Nothing, ix0)
      CeGepCode24 -> do
        (flags :: Word64) <- parseField r ix0 numeric
        let inbounds = testBit flags 0
            inrangeIdx = flags `shiftR` 1
        pure (inbounds, Just inrangeIdx, ix0 + 1)

  let loop n = do
        ty   <- getType =<< field  n    numeric
        elt  <-             field (n+1) numeric
        rest <- loop (n+2) `mplus` return []
        cxt  <- getContext
        return (Typed ty (typedValue (forwardRef cxt elt t)) : rest)
  args <- loop ix1
  (ptr, args') <-
    case args of
      [] -> fail "Invalid constant GEP with no operands"
      (base:args') -> pure (base, args')

  baseTy <-
    case mbBaseTy of
      Just baseTy -> pure baseTy
      Nothing -> Assert.elimPtrTo "constant GEP not headed by pointer" (typedType ptr)

  return $! ValConstExpr (ConstGEP isInbounds mInrangeIdx baseTy ptr args')

parseWideInteger :: Record -> Int -> Parse Integer
parseWideInteger r idx = do
  limbs <- parseSlice r idx (length (recordFields r) - idx) signedWord64
  return (foldr (\l acc -> acc `shiftL` 64 + (toInteger l)) 0 limbs)

resolveNull :: Type -> Parse PValue
resolveNull ty = case typeNull ty of
  HasNull nv    -> return nv
  ResolveNull i -> resolveNull =<< getType' =<< getTypeId i

-- | The different codes for inline @asm@ constants. Each one has minor
-- differences in how they are parsed.
data InlineAsmCode
  = InlineAsmCode18
    -- ^ @CST_CODE_INLINEASM_OLD = 18@. The original.
  | InlineAsmCode23
    -- ^ @CST_CODE_INLINEASM_OLD2 = 23@. This adds an @asmdialect@ field.
  | InlineAsmCode28
    -- ^ @CST_CODE_INLINEASM_OLD3 = 28@. This adds an @unwind@ field (which is
    -- referred to as @canThrow@ in the LLVM source code).
  | InlineAsmCode30
    -- ^ @CST_CODE_INLINEASM = 30@. This adds an explicit function type field.

-- | Parse a 'ValAsm' value. There are several variations on this theme that are
-- captured in the 'InlineAsmCode' argument.
parseInlineAsm :: InlineAsmCode -> Parse Type -> Record -> Parse (Typed PValue)
parseInlineAsm code getTy r = do
  let field = parseField r

  -- If using InlineAsmCode30 or later, we parse the type as an explicit
  -- field.
  let parseTy  = do ty <- getType =<< field 0 numeric
                    return (PtrTo ty, 1)
  -- If using an older InlineAsmCode, then we retrieve the type from the
  -- current context.
  let useCurTy = do ty <- getTy
                    return (ty, 0)
  (ty, ix) <- case code of
                InlineAsmCode18 -> useCurTy
                InlineAsmCode23 -> useCurTy
                InlineAsmCode28 -> useCurTy
                InlineAsmCode30 -> parseTy

  mask <- field ix numeric

  let test = testBit (mask :: Word32)
      hasSideEffects = test 0
      isAlignStack   = test 1
      -- We don't store these in the llvm-pretty AST at the moment:
      _asmDialect    = test 2 -- Only with InlineAsmCode23 or later
      _canThrow      = test 3 -- Only with InlineAsmCode28 or later

  asmStrSize <- field (ix + 1) numeric
  Assert.recordSizeGreater r (ix + 1 + asmStrSize)

  constStrSize <- field (ix + 2 + asmStrSize) numeric
  Assert.recordSizeGreater r (ix + 2 + asmStrSize + constStrSize)

  asmStr   <- fmap UTF8.decode $ parseSlice r (ix + 2)              asmStrSize   char
  constStr <- fmap UTF8.decode $ parseSlice r (ix + 3 + asmStrSize) constStrSize char

  let val = ValAsm hasSideEffects isAlignStack asmStr constStr

  return (Typed ty val)


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

-- fp80 is double extended format.  This conforms to IEEE 754, but is
-- store as two values: the significand and the exponent.  Discussion
-- here is relative to information from the LLVM source based at
-- https://github.com/llvm-mirror/llvm/blob/release_60 (hereafter
-- identified as LGH).
--
-- The exponent range is 16383..-16384 (14 bits), and the precision
-- (significand bits) is 64, including the integer bit (see
-- LGH/lib/Support/APFloat.cpp:75).
--
-- When reading the Record here, there are two fields, one of 65 bits
-- and the other of up to 20 bits (which clearly adds to more than
-- 80... extras are ignored).  Bits are not stored in the expected way
-- and "compensation" is needed. First the two record fields are
-- combined into an 80-bit integer (see
-- LGH/lib/Bitcode/Reader/BitcodeReader.cpp:2196-2202), using only 64
-- bits of the first field and 16 bits of the second field, discarding
-- the extra bits.  This is the result of this build operation; if
-- this result is used semantically, it should be analyzed as per
-- LGH/lib/Support/APFloat.cpp:3076-3108.

fp80build :: Type -> Record -> [Typed PValue] -> Parse Type
          -> Parse (Parse Type, [Typed PValue])
fp80build ty r cs getTy =
  do v1 <- parseField r 0 fieldLiteral
     v2 <- parseField r 1 fieldLiteral
     let -- Note bs1 <> bs2 results in bs2|bs1 layout, shifting bs2 to higher bits
         v64_0 = BitS.take (Bits' 64)
                 $ BitS.take (Bits' 16) v2 `BitS.joinBitString` v1
         v64_1 = BitS.drop (Bits' 48) v2
         -- result is v64_1|v64_0 being v0|v1
         fullexp :: Word16
         fullexp = BitS.fromBitString $ BitS.take (Bits' 16) v64_1 -- includes sign bit
         significnd :: Word64
         significnd = BitS.fromBitString $ v64_0
         fp80Val = FP80_LongDouble fullexp significnd
     return (getTy, Typed ty (ValFP80 fp80Val):cs)
