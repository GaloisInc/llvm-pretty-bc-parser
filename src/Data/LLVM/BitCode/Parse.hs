{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

module Data.LLVM.BitCode.Parse where

import           Text.LLVM.AST
import           Text.LLVM.PP

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), unless)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail -- makes fail visible for instance
#endif
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Except (MonadError(..), Except, runExcept)
import           Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import           Control.Monad.State.Strict (MonadState(..), StateT(..)
                                            , gets, modify)
import qualified Data.Foldable as F
import           Data.Maybe (fromMaybe)
import           Data.Semigroup
import           Data.Word ( Word32 )

import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import qualified Control.Exception as X
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           GHC.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.PrettyPrint.HughesPJClass as PP

import           Prelude


-- Error Collection Parser -----------------------------------------------------

data Error = Error
  { errContext :: [String]
  , errMessage :: String
  } deriving (Show, Eq, Ord)

formatError :: Error -> String
formatError err
  | null (errContext err) = errMessage err
  | otherwise             = unlines
                          $ errMessage err
                          : formatContext (errContext err)

formatContext :: [String] -> [String]
formatContext cxt = "from:" : map ('\t' :) cxt

newtype Parse a = Parse
  { unParse :: ReaderT Env (StateT ParseState (Except Error)) a
  } deriving ( Functor, Applicative, MonadFix
             , MonadReader Env
             , MonadState ParseState
             , MonadError Error
             )

instance Monad Parse where
#if !MIN_VERSION_base(4,11,0)
  {-# INLINE return #-}
  return = pure
#endif

  {-# INLINE (>>=) #-}
  Parse m >>= f = Parse (m >>= unParse . f)

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail = failWithContext
#endif

instance MonadFail Parse where
  {-# INLINE fail #-}
  fail = failWithContext

instance Alternative Parse where
  {-# INLINE empty #-}
  empty = failWithContext "empty"

  {-# INLINE (<|>) #-}
  a <|> b = Parse $ catchError (unParse a) (const (unParse b))

instance MonadPlus Parse where
  {-# INLINE mzero #-}
  mzero = failWithContext "mzero"

  {-# INLINE mplus #-}
  mplus = (<|>)

runParse :: Parse a -> Either Error (a, ParseState)
runParse (Parse m) =
  case runExcept (runStateT (runReaderT m emptyEnv) emptyParseState) of
    Left err  -> Left err
    Right res -> Right res

notImplemented :: Parse a
notImplemented  = fail "not implemented"

-- Parse State -----------------------------------------------------------------

data ParseState = ParseState
  { psTypeTable     :: TypeTable
  , psTypeTableSize :: !Int
  , psValueTable    :: ValueTable
  , psStringTable   :: Maybe StringTable
  , psMdTable       :: ValueTable
  , psMdRefs        :: MdRefTable
  , psFunProtos     :: Seq.Seq FunProto
  , psNextResultId  :: !Int
  , psTypeName      :: Maybe String
  , psNextTypeId    :: !Int
  , psLastLoc       :: Maybe PDebugLoc
  , psKinds         :: !KindTable
  , psModVersion    :: !Int
  , psWarnings      :: Seq.Seq ParseWarning
  } deriving (Show)

-- | The initial parsing state.
emptyParseState :: ParseState
emptyParseState  = ParseState
  { psTypeTable     = IntMap.empty
  , psTypeTableSize = 0
  , psValueTable    = emptyValueTable False
  , psStringTable   = Nothing
  , psMdTable       = emptyValueTable False
  , psMdRefs        = IntMap.empty
  , psFunProtos     = Seq.empty
  , psNextResultId  = 0
  , psTypeName      = Nothing
  , psNextTypeId    = 0
  , psLastLoc       = Nothing
  , psKinds         = emptyKindTable
  , psModVersion    = 0
  , psWarnings      = Seq.empty
  }

-- | The next implicit result id.
nextResultId :: Parse Int
nextResultId  = Parse $ do
  ps <- get
  put ps { psNextResultId = psNextResultId ps + 1 }
  return (psNextResultId ps)

type PDebugLoc = DebugLoc' Int

setLastLoc :: PDebugLoc -> Parse ()
setLastLoc loc = Parse $ do
  ps <- get
  put $! ps { psLastLoc = Just loc }

setRelIds :: Bool -> Parse ()
setRelIds b = Parse $ do
  ps <- get
  put $! ps { psValueTable = (psValueTable ps) { valueRelIds = b }}

getRelIds :: HasValueTable m => m Bool
getRelIds  = valueRelIds <$> getValueTable

getLastLoc :: Parse PDebugLoc
getLastLoc  = do
  ps <- Parse get
  case psLastLoc ps of
    Just loc -> return loc
    Nothing  -> fail "No last location available"

setModVersion :: Int -> Parse ()
setModVersion v = Parse $ do
  ps <- get
  put $! ps { psModVersion = v }

getModVersion :: Parse Int
getModVersion = Parse (psModVersion <$> get)

-- | Sort of a hack to preserve state between function body parses.  It would
-- really be nice to separate this into a different monad, that could just run
-- under the Parse monad, but sort of unnecessary in the long run.
enterFunctionDef :: Parse a -> Parse a
enterFunctionDef m = Parse $ do
  ps  <- get
  put ps
    { psNextResultId = 0
    }
  res <- unParse m
  ps' <- get
  put ps'
    { psValueTable = psValueTable ps
    , psMdTable    = psMdTable ps
    , psMdRefs     = psMdRefs ps
    , psLastLoc    = Nothing
    }
  return res


-- Type Table ------------------------------------------------------------------

type TypeTable = IntMap.IntMap Type

-- | Generate a type table, and a type symbol table.
mkTypeTable :: [Type] -> TypeTable
mkTypeTable  = IntMap.fromList . zip [0 ..]

-- | Exceptions contain a callstack, parsing context, explanation, and index
data BadForwardRef
  = BadTypeRef  CallStack [String] String Int
  | BadValueRef CallStack [String] String Int
    deriving (Show)

instance X.Exception BadForwardRef

badRefError :: BadForwardRef -> Error
badRefError ref =
  let (stk, cxt, explanation, i, thing) =
        case ref of
          BadTypeRef  stk' cxt' explanation' i' -> (stk', cxt', explanation', i', "type")
          BadValueRef stk' cxt' explanation' i' -> (stk', cxt', explanation', i', "value")
  in Error cxt $ unlines ["bad forward reference to " ++ thing ++ ": " ++ show i
                         , "additional details: "
                         , explanation
                         , "with call stack: "
                         , prettyCallStack stk
                         ]

-- | As type tables are always pre-allocated, looking things up should never
-- fail.  As a result, the worst thing that could happen is that the type entry
-- causes a runtime error.  This is pretty bad, but it's an acceptable trade-off
-- for the complexity of the forward references in the type table.
lookupTypeRef :: HasCallStack
              => [String] -> Int -> TypeTable -> Type
lookupTypeRef cxt n =
  let explanation = "Bad reference into type table"
  in fromMaybe (X.throw (BadTypeRef callStack cxt explanation n)) . IntMap.lookup n

setTypeTable :: TypeTable -> Parse ()
setTypeTable table = Parse $ do
  ps <- get
  put ps { psTypeTable = table }

getTypeTable :: Parse TypeTable
getTypeTable  = Parse (psTypeTable <$> get)

setTypeTableSize :: Int -> Parse ()
setTypeTableSize n = Parse $ do
  ps <- get
  put ps { psTypeTableSize = n }

-- | Retrieve the current type name, failing if it hasn't been set.
getTypeName :: Parse Ident
getTypeName  = Parse $ do
  ps  <- get
  str <- case psTypeName ps of
    Just tn -> do
      put ps { psTypeName = Nothing }
      return tn
    Nothing -> do
      put ps { psNextTypeId = psNextTypeId ps + 1 }
      return (show (psNextTypeId ps))
  return (Ident str)

setTypeName :: String -> Parse ()
setTypeName name = Parse $ do
  ps <- get
  put ps { psTypeName = Just name }

-- | Lookup the value of a type; don't attempt to resolve to an alias.
getType' :: Int -> Parse Type
getType' ref = do
  ps <- Parse get
  unless (ref < psTypeTableSize ps)
    (fail ("type reference " ++ show ref ++ " is too large"))
  cxt <- getContext
  return (lookupTypeRef cxt ref (psTypeTable ps))

-- | Test to see if the type table has been added to already.
isTypeTableEmpty :: Parse Bool
isTypeTableEmpty  = Parse (IntMap.null . psTypeTable <$> get)

setStringTable :: StringTable -> Parse ()
setStringTable st = Parse $ do
  ps <- get
  put ps { psStringTable = Just st }

getStringTable :: Parse (Maybe StringTable)
getStringTable = Parse (psStringTable <$> get)

-- Value Tables ----------------------------------------------------------------

-- | Values that have an identifier instead of a string label
type PValue = Value' Int

type PInstr = Instr' Int

data ValueTable = ValueTable
  { valueNextId  :: !Int
  , valueEntries :: IntMap.IntMap (Typed PValue)
  , strtabEntries :: IntMap.IntMap (Int, Int)
  , valueRelIds  :: Bool
  } deriving (Show)

emptyValueTable :: Bool -> ValueTable
emptyValueTable rel = ValueTable
  { valueNextId  = 0
  , valueEntries = IntMap.empty
  , strtabEntries = IntMap.empty
  , valueRelIds  = rel
  }

addValue :: Typed PValue -> ValueTable -> ValueTable
addValue tv vs = snd (addValue' tv vs)

addValue' :: Typed PValue -> ValueTable -> (Int,ValueTable)
addValue' tv vs = (valueNextId vs,vs')
  where
  vs' = vs
    { valueNextId  = valueNextId vs + 1
    , valueEntries = IntMap.insert (valueNextId vs) tv (valueEntries vs)
    }

-- | Push a value into the value table, and return its index.
pushValue :: Typed PValue -> Parse Int
pushValue tv = Parse $ do
  ps <- get
  let vt = psValueTable ps
  put ps { psValueTable = addValue tv vt }
  return (valueNextId vt)

-- | Get the index for the next value.
nextValueId :: Parse Int
nextValueId = valueNextId <$> getValueTable

-- | Depending on whether or not relative ids are in use, adjust the id.
adjustId :: Int -> Parse Int
adjustId n = do
  vt <- getValueTable
  return (translateValueId vt n)

-- | Translate an id, relative to the value table it references.
-- NOTE: The relative conversion has to be done on a Word32 to handle overflow
-- when n is large the same way BitcodeReaderMDValueList::getValue does.
translateValueId :: ValueTable -> Int -> Int
translateValueId vt n | valueRelIds vt = fromIntegral adjusted
                      | otherwise      = n
  where
  adjusted :: Word32
  adjusted  = fromIntegral (valueNextId vt - n)

-- | Lookup an absolute address in the value table.
lookupValueTableAbs :: Int -> ValueTable -> Maybe (Typed PValue)
lookupValueTableAbs n values = IntMap.lookup n (valueEntries values)

-- | When you know you have an absolute index.
lookupValueAbs :: HasValueTable m => Int -> m (Maybe (Typed PValue))
lookupValueAbs n = lookupValueTableAbs n `fmap` getValueTable

-- | Lookup either a relative id, or an absolute id.
lookupValueTable :: Int -> ValueTable -> Maybe (Typed PValue)
lookupValueTable n values =
  lookupValueTableAbs (translateValueId values n) values

-- | Lookup a value in the value table.
lookupValue :: Int -> Parse (Maybe (Typed PValue))
lookupValue n = lookupValueTable n `fmap` getValueTable

-- | Lookup lazily, hiding an error in the result if the entry doesn't exist by
-- the time it's needed.  NOTE: This always looks up an absolute index, never a
-- relative one.
forwardRef :: HasCallStack
           => [String] -> Int -> ValueTable -> Typed PValue
forwardRef cxt n vt =
  let explanation = "Bad reference into a value table"
  in fromMaybe (X.throw (BadValueRef callStack cxt explanation n)) (lookupValueTableAbs n vt)

-- | Require that a value be present.
requireValue :: Int -> Parse (Typed PValue)
requireValue n = do
  mb <- lookupValue n
  case mb of
    Just tv -> return tv
    Nothing -> fail ("value " ++ show n ++ " is not defined")

class Monad m => HasValueTable m where
  -- | Get the current value table.
  getValueTable :: m ValueTable

instance HasValueTable Parse where
  getValueTable  = gets psValueTable

-- | Retrieve the name for the next value.  Note that this doesn't assume that
-- the name gets used, and doesn't update the next id in the value table.
getNextId :: Parse Int
getNextId  = valueNextId <$> getValueTable

-- | Set the current value table.
setValueTable :: ValueTable -> Parse ()
setValueTable vt = Parse $ do
  ps <- get
  put ps { psValueTable = vt }

-- | Update the value table, giving a lazy reference to the final table.
fixValueTable :: (ValueTable -> Parse (a,[Typed PValue])) -> Parse a
fixValueTable k = do
  vt <- getValueTable
  rec let vt' = foldr addValue vt vs
      (a,vs) <- k vt'
  setValueTable vt'
  return a

fixValueTable_ :: (ValueTable -> Parse [Typed PValue]) -> Parse ()
fixValueTable_ k = fixValueTable $ \ vt -> do
  vs <- k vt
  return ((),vs)


type PValMd = ValMd' Int

type MdTable = ValueTable

class Monad m => HasMdTable m where
  getMdTable :: m MdTable

instance HasMdTable Parse where
  getMdTable = gets psMdTable

setMdTable :: MdTable -> Parse ()
setMdTable md = modify $ \ps -> ps { psMdTable = md }

getMetadata :: Int -> Parse (Typed PValMd)
getMetadata ix = do
  ps <- Parse get
  case resolveMd ix ps of
    Just tv -> case typedValue tv of
      ValMd val -> return tv { typedValue = val }
      _         -> fail "unexpected non-metadata value in metadata table"
    Nothing -> fail ("metadata index " ++ show ix ++ " is not defined")

resolveMd :: Int -> ParseState -> Maybe (Typed PValue)
resolveMd ix ps = nodeRef `mplus` mdValue
  where
  reference = Typed (PrimType Metadata) . ValMd . ValMdRef
  nodeRef   = reference `fmap` IntMap.lookup ix (psMdRefs ps)
  mdValue   = lookupValueTableAbs ix (psMdTable ps)


type MdRefTable = IntMap.IntMap Int

setMdRefs :: MdRefTable -> Parse ()
setMdRefs refs = Parse $ do
  ps <- get
  put $! ps { psMdRefs = refs `IntMap.union` psMdRefs ps }


-- Function Prototypes ---------------------------------------------------------

data FunProto = FunProto
  { protoType       :: Type
  , protoLinkage    :: Maybe Linkage
  , protoVisibility :: Maybe Visibility
  , protoGC         :: Maybe GC
  , protoSym        :: Symbol
  , protoIndex      :: Int
  , protoSect       :: Maybe String
  , protoComdat     :: Maybe String
  } deriving Show

-- | Push a function prototype on to the prototype stack.
pushFunProto :: FunProto -> Parse ()
pushFunProto p = Parse $ do
  ps <- get
  put ps { psFunProtos = psFunProtos ps Seq.|> p }

-- | Take a single function prototype off of the prototype stack.
popFunProto :: Parse FunProto
popFunProto  = do
  ps <- Parse get
  case Seq.viewl (psFunProtos ps) of
    Seq.EmptyL   -> fail "empty function prototype stack"
    p Seq.:< ps' -> do
      Parse (put ps { psFunProtos = ps' })
      return p


-- Parsing Environment ---------------------------------------------------------

-- | The Reader environment information maintained in the 'Parse' monad.
data Env = Env
  { envSymtab  :: Symtab  -- ^ the global symbol table
  , envContext :: [String] -- ^ the stack of "label" strings (a "stacktrace")
  } deriving Show

emptyEnv :: Env
emptyEnv  = Env
  { envSymtab  = mempty
  , envContext = mempty
  }

-- | Extend the symbol table for an environment, yielding a new environment.
extendSymtab :: Symtab -> Env -> Env
extendSymtab symtab env = env { envSymtab = envSymtab env `mappend` symtab }

-- | Add a label to the context of an environment, yielding a new environment.
addLabel :: String -> Env -> Env
addLabel l env = env { envContext = l : envContext env }

class Monad m => HasParseEnv m where
  -- | Gets the "stacktrace" for what is currently being evaluated (as set by the
  -- 'label' function, which calls 'addLabel' above).  Note that the label
  -- referenced here is the parsing processing notation, and NOT the llvm-pretty
  -- AST 'lab' type argument which references the Basic Block label and which is
  -- set with the 'llvm-pretty.relabel' function... an unfortunate overloading of
  -- the term "label".
  getContext :: m [String]
  -- | Retrieve the value symbol table
  getValueSymtab :: m ValueSymtab

instance HasParseEnv Parse where
  getContext = asks envContext
  getValueSymtab = asks $ symValueSymtab . envSymtab

data Symtab = Symtab
  { symValueSymtab :: ValueSymtab
  , symTypeSymtab  :: TypeSymtab
  } deriving (Show)

instance Semigroup Symtab where
  l <> r = Symtab
    { symValueSymtab = symValueSymtab l <> symValueSymtab r
    , symTypeSymtab  = symTypeSymtab  l <> symTypeSymtab  r
    }

instance Monoid Symtab where
  mempty = Symtab
    { symValueSymtab = emptyValueSymtab
    , symTypeSymtab  = mempty
    }

  mappend = (<>)

withSymtab :: Symtab -> Parse a -> Parse a
withSymtab symtab body = Parse $ do
  local (extendSymtab symtab) (unParse body)

-- | Run a computation with an extended value symbol table.
withValueSymtab :: ValueSymtab -> Parse a -> Parse a
withValueSymtab symtab = withSymtab (mempty { symValueSymtab = symtab })

-- | Run a computation with an extended type symbol table.
withTypeSymtab :: TypeSymtab -> Parse a -> Parse a
withTypeSymtab symtab = withSymtab (mempty { symTypeSymtab = symtab })

-- | Retrieve the type symbol table.
getTypeSymtab :: Parse TypeSymtab
getTypeSymtab  = Parse (symTypeSymtab . envSymtab <$> ask)

-- | Label a sub-computation with its context.
label :: String -> Parse a -> Parse a
label l m = Parse $ do
  local (addLabel l) (unParse m)

-- | Fail, taking into account the current context.
failWithContext :: String -> Parse a
failWithContext msg = Parse $ do
  env <- ask
  throwError Error
    { errMessage = msg
    , errContext = envContext env
    }

-- | Attempt to find the type id in the type symbol table, when that fails,
-- look it up in the type table.
getType :: Int -> Parse Type
getType ref = do
  symtab <- getTypeSymtab
  case IntMap.lookup ref (tsById symtab) of
    Just i  -> return (Alias i)
    Nothing -> getType' ref

-- | Find the id associated with a type alias.
getTypeId :: Ident -> Parse Int
getTypeId n = do
  symtab <- getTypeSymtab
  case Map.lookup n (tsByName symtab) of
    Just ix -> return ix
    Nothing -> fail ("unknown type alias " ++ show (ppLLVM llvmVlatest (llvmPP n)))


-- Value Symbol Table ----------------------------------------------------------

-- | An LLVM Bitcode file, it is comprised of nested Blocks of information,
-- with records available at each block.  Different blocks hold different
-- types of information, and the nesting represents program scope and what is
-- defined/accessible at a particular point within the program.
--
-- The `Parse` and `Finalize` monads maintain a set of symbol tables to use
-- for lookups, as stored in the `Env` structure referenced by those monads:
--
--   * `valSymtab` for value references
--
--   * `fnSymtab` for function references
--
--   * `bbSymTab` for basic-block references, either "named" by association
--     with a symbol name (like the function's entry block) or "anonymous",
--     where there is no direct symbol but there may be a block label (like
--     for a goto statement or similar surface language construct.)
--
-- These lookups are necessary to "relabel" values that reference symbol or
-- label addresses in the code with the actual targets as resolved by
-- processing the entirety of the LLVM bitcode file.  The
-- llvm-pretty-bc-parser runs in two phases: the initial phase where it
-- processes the LLVM bitcode stream to create "Partial" representations of
-- all of the elements, followed by a "finalization" phase where it performs
-- all of the label references above (via the llvm-pretty `relabel`
-- operation), as well as other fixups to convert the "Partial" data
-- structures into the structures defined by the `llvm-pretty` AST.

data ValueSymtab =
  ValueSymtab
  { valSymtab :: IntMap.IntMap SymName
  , bbSymtab  :: IntMap.IntMap SymName
  , fnSymtab  :: IntMap.IntMap SymName
  } deriving (Show)

type SymName = Either String Int

instance Semigroup ValueSymtab where
  l <> r = ValueSymtab
    { valSymtab = valSymtab l `IntMap.union` valSymtab r
    , bbSymtab  = bbSymtab l  `IntMap.union` bbSymtab r
    , fnSymtab  = fnSymtab l  `IntMap.union` fnSymtab r
    }

instance Monoid ValueSymtab where
  mappend = (<>)
  mempty = ValueSymtab
    { valSymtab = IntMap.empty
    , bbSymtab  = IntMap.empty
    , fnSymtab  = IntMap.empty
    }

renderName :: SymName -> String
renderName  = either id show

mkBlockLabel :: SymName -> BlockLabel
mkBlockLabel  = either (Named . Ident) Anon

emptyValueSymtab :: ValueSymtab
emptyValueSymtab  = mempty

addEntry :: Int -> String -> ValueSymtab -> ValueSymtab
addEntry i n t = t { valSymtab = IntMap.insert i (Left n) (valSymtab t) }

addBBEntry :: Int -> String -> ValueSymtab -> ValueSymtab
addBBEntry i n t = t { bbSymtab = IntMap.insert i (Left n) (bbSymtab t) }

addBBAnon :: Int -> Int -> ValueSymtab -> ValueSymtab
addBBAnon i n t = t { bbSymtab = IntMap.insert i (Right n) (bbSymtab t) }

addFNEntry :: Int -> Int -> String -> ValueSymtab -> ValueSymtab
-- TODO: do we ever need to be able to look up the offset?
addFNEntry i _o n t = t { fnSymtab = IntMap.insert i (Left n) (fnSymtab t) }

addFwdFNEntry :: Int -> Int -> ValueSymtab -> ValueSymtab
addFwdFNEntry i o t = t { fnSymtab = IntMap.insert i (Right o) (fnSymtab t) }

-- | Lookup the name of an entry. Returns @Nothing@ when it's not present.
entryNameMb :: HasParseEnv m => Int -> m (Maybe String)
entryNameMb n = do
  symtab <- getValueSymtab
  return $! fmap renderName
         $  IntMap.lookup n (valSymtab symtab) `mplus`
            IntMap.lookup n (fnSymtab symtab)

-- | Lookup the name of an entry.
entryName :: (HasParseEnv m, HasValueTable m, MonadFail m) => Int -> m String
entryName n = do
  mentry <- entryNameMb n
  case mentry of
    Just name -> return name
    Nothing   ->
      do isRel  <- getRelIds
         symtab <- getValueSymtab
         fail $ unlines
           [ "entry " ++ show n ++ (if isRel then " (relative)" else "")
              ++ " is missing from the symbol table"
           , show symtab ]

-- | Lookup the name of a basic block.
bbEntryName :: Maybe Symbol -> Int -> Finalize (Maybe BlockLabel)
bbEntryName mbSym n =
  fmap mkBlockLabel
  <$> case mbSym of
        Just fn -> do
          -- Lookup entry in a function (Defines) symbol table
          funcSyms <- asks parsedFuncSymtabs
          return (IntMap.lookup n . bbSymtab =<< Map.lookup fn funcSyms)
        Nothing -> do
          -- Lookup entry in global (top-level) symbol table
          symtab <- getValueSymtab
          return (IntMap.lookup n (bbSymtab symtab))

-- | Lookup the name of a basic block.
requireBbEntryName :: Maybe Symbol -> Int -> Finalize BlockLabel
requireBbEntryName mbSym n = do
  mb <- bbEntryName mbSym n
  case mb of
    Just l  -> return l
    Nothing -> fail ("basic block " ++ show n ++ " / " ++ show mbSym ++ " has no id")

-- Type Symbol Tables ----------------------------------------------------------

data TypeSymtab = TypeSymtab
  { tsById   :: IntMap.IntMap Ident
  , tsByName :: Map.Map Ident Int
  } deriving Show

instance Semigroup TypeSymtab where
  l <> r = TypeSymtab
    { tsById   = tsById   l `IntMap.union` tsById r
    , tsByName = tsByName l `Map.union` tsByName r
    }

instance Monoid TypeSymtab where
  mempty = TypeSymtab
    { tsById   = IntMap.empty
    , tsByName = Map.empty
    }

  mappend = (<>)

addTypeSymbol :: Int -> Ident -> TypeSymtab -> TypeSymtab
addTypeSymbol ix n ts = ts
  { tsById   = IntMap.insert ix n (tsById ts)
  , tsByName = Map.insert n ix (tsByName ts)
  }


-- Metadata Kind Table ---------------------------------------------------------

data KindTable = KindTable
  { ktNames :: IntMap.IntMap String
  } deriving (Show)

emptyKindTable :: KindTable
emptyKindTable  = KindTable
  { ktNames = IntMap.fromList
    [ (0, "dbg"   )
    , (1, "tbaa"  )
    , (2, "prof"  )
    , (3, "fpmath")
    , (4, "range" )
    ]
  }

addKind :: Int -> String -> Parse ()
addKind kind name = Parse $ do
  ps <- get
  let KindTable { .. } = psKinds ps
  put $! ps { psKinds = KindTable { ktNames = IntMap.insert kind name ktNames } }

getKind :: Int -> Parse String
getKind kind = do
  ps <- Parse get
  let KindTable { .. } = psKinds ps
  case IntMap.lookup kind ktNames of
    Just name -> return name
    Nothing   -> fail ("Unknown kind id: " ++ show kind ++ "\nKind table: " ++ show (psKinds ps))

-- Partial Symbols -------------------------------------------------------------

newtype StringTable = Strtab BS.ByteString
  deriving (Show)
--newtype SymbolTable = Symtab BS.ByteString

mkStrtab :: BS.ByteString -> StringTable
mkStrtab = Strtab

--mkSymtab :: BS.ByteString -> SymbolTable
--mkSymtab = Symtab

resolveStrtabSymbol :: StringTable -> Int -> Int -> Symbol
resolveStrtabSymbol (Strtab bs) start len =
  Symbol $ UTF8.decode $ BS.unpack $ BS.take len $ BS.drop start bs

-- Parser Warnings -------------------------------------------------------------

-- | Warnings about non-fatal issues that arise during parsing.
data ParseWarning
  = -- | The parser encountered a metadata record with an unexpected size.
    -- The 'Int' is the actual record size that was encountered, the
    -- 'MetadataRecordSizeRange' is the expected range of possible sizes, and
    -- the @[String]@ is the stack trace at the point where the warning was
    -- emitted.
    InvalidMetadataRecordSize !Int !MetadataRecordSizeRange ![String]
  deriving Show

-- | The expected size of a metadata record.
data MetadataRecordSizeRange
  = -- | The size is expected between a lower bound (the first 'Int') and an
    -- upper bound (the second 'Int'), inclusive.
    MetadataRecordSizeBetween !Int !Int

  | -- | The size is expected to be within a certain list of possible values.
    MetadataRecordSizeIn ![Int]

  | -- The size is expected to be greater than or equal to a certain value.
    MetadataRecordSizeAtLeast !Int
  deriving Show

-- | Append a 'ParseWarning' to the end of the currently accumulated warnings.
addParseWarning :: ParseWarning -> Parse ()
addParseWarning pw = Parse $ do
  ps <- get
  put $! ps { psWarnings = psWarnings ps Seq.|> pw }

-- | Pretty-print a single 'ParseWarning' in a format suitable for user-facing
-- messages. (See also 'ppParseWarnings', which pretty-prints several
-- 'ParseWarnings's in a cohesive way.)
ppParseWarning :: ParseWarning -> PP.Doc
ppParseWarning (InvalidMetadataRecordSize len range cxt) =
  PP.vcat $
    [ "Invalid record size:" PP.<+> PP.pPrint len
    , expectedSizeMsg
    ] ++ map PP.text (formatContext cxt)
  where
    expectedSizeMsg :: PP.Doc
    expectedSizeMsg =
      case range of
        MetadataRecordSizeBetween lb ub ->
          "Expected size between" PP.<+> PP.pPrint lb
            PP.<+> "and" PP.<+> PP.pPrint ub
        MetadataRecordSizeIn ns ->
          "Expected one of:" PP.<+> PP.pPrint ns
        MetadataRecordSizeAtLeast lb ->
          "Expected size of" PP.<+> PP.pPrint lb PP.<+> "or greater"

-- | Pretty-print a group of 'ParseWarning's in a format suitable for
-- user-facing messages.
ppParseWarnings :: Seq.Seq ParseWarning -> PP.Doc
ppParseWarnings warnings
  | null warnings
  = PP.empty
  | otherwise
  = PP.vcat $
      ["Encountered the following warnings during parsing:"] ++
      map
        (\warning ->
          PP.nest 4 $ PP.vcat [ppParseWarning warning, ""])
        (F.toList warnings) ++
      [supportMsg | any isInvalidMetadataRecordSize warnings]
  where
    isInvalidMetadataRecordSize :: ParseWarning -> Bool
    isInvalidMetadataRecordSize (InvalidMetadataRecordSize {}) = True

    supportMsg :: PP.Doc
    supportMsg =
      PP.vcat
        [ "Are you sure you're using a supported version of LLVM/Clang?"
        , "Check here: https://github.com/GaloisInc/llvm-pretty-bc-parser"
        ]

-- Finalize Monad --------------------------------------------------------------

-- | During the "finalization" pass, all references should be resolved, including
-- actual Block label value references, which may be to either global or
-- function-local targets.  The 'Finalize' Monad provides access to the tables
-- needed to perform this resolution via the 'FinalizeEnv' data in a Reader monad
-- context.

data FinalizeEnv = FinalizeEnv
                   { parsedEnv :: Env
                   , parsedMdTable :: ValueTable
                   , parsedValueTable :: ValueTable
                   , parsedFuncSymtabs :: FuncSymTabs
                   }

type FuncSymTabs = Map.Map Symbol ValueSymtab

newtype Finalize a = Finalize
  { unFinalize :: ReaderT FinalizeEnv (Except Error) a
  } deriving (Functor, Applicative, MonadReader FinalizeEnv)

instance Monad Finalize where
#if !MIN_VERSION_base(4,11,0)
  {-# INLINE return #-}
  return = pure
#endif

  {-# INLINE (>>=) #-}
  Finalize m >>= f = Finalize (m >>= unFinalize . f)

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail = failWithContext'
#endif

instance MonadFail Finalize where
  {-# INLINE fail #-}
  fail = failWithContext'

instance Alternative Finalize where
  {-# INLINE empty #-}
  empty = failWithContext' "empty"

  {-# INLINE (<|>) #-}
  a <|> b = Finalize $ catchError (unFinalize a) (const (unFinalize b))

instance MonadPlus Finalize where
  {-# INLINE mzero #-}
  mzero = failWithContext' "mzero"

  {-# INLINE mplus #-}
  mplus = (<|>)

instance HasParseEnv Finalize where
  getContext = asks $ envContext . parsedEnv
  getValueSymtab = asks $ symValueSymtab . envSymtab . parsedEnv

instance HasMdTable Finalize where
  getMdTable = asks parsedMdTable

instance HasValueTable Finalize where
  getValueTable = asks parsedValueTable

-- | Fail, taking into account the current context.
failWithContext' :: String -> Finalize a
failWithContext' msg =
  Finalize $
  do env <- asks parsedEnv
     throwError Error
       { errMessage = msg
       , errContext = envContext env
       }

-- | Run a Finalize Monad operation in the context of a Parse monad.  The
-- information for the 'FinalizeEnv' is obtained from the Parse monad's 'Env',
-- plus the VALUE_SYMTAB_BLOCK information for each function as mapped by the
-- function's name.

liftFinalize :: FuncSymTabs -> Finalize a -> Parse a
liftFinalize defs (Finalize m) =
  do env <- ask
     mdt <- getMdTable
     valt <- getValueTable
     let fenv = FinalizeEnv { parsedEnv = env
                            , parsedMdTable = mdt
                            , parsedValueTable = valt
                            , parsedFuncSymtabs = defs
                            }
     case runExcept (runReaderT m fenv) of
       Left err -> throwError err
       Right a -> return a
