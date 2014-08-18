{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Data.LLVM.BitCode.Parse where

import Text.LLVM.AST

import Control.Applicative (Applicative(..),Alternative(..),(<$>))
import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Data.Word ( Word32 )
import MonadLib
import qualified Control.Exception as X
import qualified Data.Map as Map
import qualified Data.Sequence as Seq


-- Error Collection Parser -----------------------------------------------------

data Error = Error
  { errContext :: [String]
  , errMessage :: String
  } deriving (Show)

formatError :: Error -> String
formatError err
  | null (errContext err) = errMessage err
  | otherwise             = unlines
                          $ errMessage err
                          : "from:"
                          : map ('\t' :) (errContext err)

newtype Parse a = Parse
  { unParse :: ReaderT Env (StateT ParseState (ExceptionT Error Lift)) a
  } deriving (Functor,Applicative,MonadFix)

instance Monad Parse where
  {-# INLINE return #-}
  return  = Parse . return

  {-# INLINE (>>=) #-}
  Parse m >>= f = Parse (m >>= unParse . f)

  {-# INLINE fail #-}
  fail = failWithContext

instance Alternative Parse where
  {-# INLINE empty #-}
  empty = failWithContext "empty"

  {-# INLINE (<|>) #-}
  a <|> b = Parse (either (const (unParse b)) return =<< try (unParse a))

instance MonadPlus Parse where
  {-# INLINE mzero #-}
  mzero = failWithContext "mzero"

  {-# INLINE mplus #-}
  mplus = (<|>)

runParse :: Parse a -> Either Error a
runParse (Parse m) = case runM m emptyEnv emptyParseState of
  Left err    -> Left err
  Right (a,_) -> Right a

notImplemented :: Parse a
notImplemented  = fail "not implemented"

-- Parse State -----------------------------------------------------------------

data ParseState = ParseState
  { psTypeTable     :: TypeTable
  , psTypeTableSize :: !Int
  , psValueTable    :: ValueTable
  , psMdTable       :: ValueTable
  , psMdRefs        :: MdRefTable
  , psFunProtos     :: Seq.Seq FunProto
  , psNextResultId  :: !Int
  , psTypeName      :: Maybe String
  , psNextTypeId    :: !Int
  , psLastLoc       :: Maybe PDebugLoc
  } deriving (Show)

-- | The initial parsing state.
emptyParseState :: ParseState
emptyParseState  = ParseState
  { psTypeTable     = Map.empty
  , psTypeTableSize = 0
  , psValueTable    = emptyValueTable False
  , psMdTable       = emptyValueTable False
  , psMdRefs        = Map.empty
  , psFunProtos     = Seq.empty
  , psNextResultId  = 0
  , psTypeName      = Nothing
  , psNextTypeId    = 0
  , psLastLoc       = Nothing
  }

-- | The next implicit result id.
nextResultId :: Parse Int
nextResultId  = Parse $ do
  ps <- get
  set ps { psNextResultId = psNextResultId ps + 1 }
  return (psNextResultId ps)

type PDebugLoc = DebugLoc' Int

setLastLoc :: PDebugLoc -> Parse ()
setLastLoc loc = Parse $ do
  ps <- get
  set $! ps { psLastLoc = Just loc }

setRelIds :: Bool -> Parse ()
setRelIds b = Parse $ do
  ps <- get
  set $! ps { psValueTable = (psValueTable ps) { valueRelIds = b }}

getRelIds :: Parse Bool
getRelIds  = Parse $ do
  ps <- get
  return (valueRelIds (psValueTable ps))

getLastLoc :: Parse PDebugLoc
getLastLoc  = Parse $ do
  ps <- get
  case psLastLoc ps of
    Just loc -> return loc
    Nothing  -> fail "No last location available"

-- | Sort of a hack to preserve state between function body parses.  It would
-- really be nice to separate this into a different monad, that could just run
-- under the Parse monad, but sort of unnecessary in the long run.
enterFunctionDef :: Parse a -> Parse a
enterFunctionDef m = Parse $ do
  ps  <- get
  set ps
    { psNextResultId = 0
    }
  res <- unParse m
  ps' <- get
  set ps'
    { psValueTable = psValueTable ps
    , psMdTable    = psMdTable ps
    , psMdRefs     = psMdRefs ps
    , psLastLoc    = Nothing
    }
  return res


-- Type Table ------------------------------------------------------------------

type TypeTable = Map.Map Int Type

-- | Generate a type table, and a type symbol table.
mkTypeTable :: [Type] -> TypeTable
mkTypeTable  = Map.fromList . zip [0 ..]

data BadForwardRef
  = BadTypeRef [String] Int
  | BadValueRef [String] Int
    deriving (Show,Typeable)

instance X.Exception BadForwardRef

badRefError :: BadForwardRef -> Error
badRefError ref = case ref of
  BadTypeRef  c i -> Error c ("bad forward reference to type: " ++ show i)
  BadValueRef c i -> Error c ("bad forward reference to value: " ++ show i)

-- | As type tables are always pre-allocated, looking things up should never
-- fail.  As a result, the worst thing that could happen is that the type entry
-- causes a runtime error.  This is pretty bad, but it's an acceptable trade-off
-- for the complexity of the forward references in the type table.
lookupTypeRef :: [String] -> Int -> TypeTable -> Type
lookupTypeRef cxt n = fromMaybe (X.throw (BadTypeRef cxt n)) . Map.lookup n

setTypeTable :: TypeTable -> Parse ()
setTypeTable table = Parse $ do
  ps <- get
  set ps { psTypeTable = table }

getTypeTable :: Parse TypeTable
getTypeTable  = Parse (psTypeTable <$> get)

setTypeTableSize :: Int -> Parse ()
setTypeTableSize n = Parse $ do
  ps <- get
  set ps { psTypeTableSize = n }

-- | Retrieve the current type name, failing if it hasn't been set.
getTypeName :: Parse Ident
getTypeName  = Parse $ do
  ps  <- get
  str <- case psTypeName ps of
    Just tn -> do
      set ps { psTypeName = Nothing }
      return tn
    Nothing -> do
      set ps { psNextTypeId = psNextTypeId ps + 1 }
      return (show (psNextTypeId ps))
  return (Ident str)

setTypeName :: String -> Parse ()
setTypeName name = Parse $ do
  ps <- get
  set ps { psTypeName = Just name }

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
isTypeTableEmpty  = Parse (Map.null . psTypeTable <$> get)


-- Value Tables ----------------------------------------------------------------

-- | Values that have an identifier instead of a string label
type PValue = Value' Int

type PInstr = Instr' Int

data ValueTable = ValueTable
  { valueNextId  :: !Int
  , valueEntries :: Map.Map Int (Typed PValue)
  , valueRelIds  :: Bool
  } deriving (Show)

emptyValueTable :: Bool -> ValueTable
emptyValueTable rel = ValueTable
  { valueNextId  = 0
  , valueEntries = Map.empty
  , valueRelIds  = rel
  }

addValue :: Typed PValue -> ValueTable -> ValueTable
addValue tv vs = snd (addValue' tv vs)

addValue' :: Typed PValue -> ValueTable -> (Int,ValueTable)
addValue' tv vs = (valueNextId vs,vs')
  where
  vs' = vs
    { valueNextId  = valueNextId vs + 1
    , valueEntries = Map.insert (valueNextId vs) tv (valueEntries vs)
    }

-- | Push a value into the value table, and return its index.
pushValue :: Typed PValue -> Parse Int
pushValue tv = Parse $ do
  ps <- get
  let vt = psValueTable ps
  set ps { psValueTable = addValue tv vt }
  return (valueNextId vt)

-- | Get the index for the next value.
nextValueId :: Parse Int
nextValueId  = Parse (valueNextId . psValueTable <$> get)

-- | Depending on whether or not relative ids are in use, adjust the id.
adjustId :: Int -> Parse Int
adjustId n = do
  vt <- getValueTable
  return (translateValueId vt n)

-- | Translate an id, relative to the value table it references.
translateValueId :: ValueTable -> Int -> Int
translateValueId vt n | valueRelIds vt = fromIntegral adjusted
                      | otherwise      = n
  where
  adjusted :: Word32
  adjusted  = fromIntegral (valueNextId vt - n)

-- | Lookup an absolute address in the value table.
lookupValueTableAbs :: Int -> ValueTable -> Maybe (Typed PValue)
lookupValueTableAbs n values = Map.lookup n (valueEntries values)

-- | When you know you have an absolute index.
lookupValueAbs :: Int -> Parse (Maybe (Typed PValue))
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
forwardRef :: [String] -> Int -> ValueTable -> Typed PValue
forwardRef cxt n vt =
  fromMaybe (X.throw (BadValueRef cxt n)) (lookupValueTableAbs n vt)

-- | Require that a value be present.
requireValue :: Int -> Parse (Typed PValue)
requireValue n = do
  mb <- lookupValue n
  case mb of
    Just tv -> return tv
    Nothing -> fail ("value " ++ show n ++ " is not defined")

-- | Get the current value table.
getValueTable :: Parse ValueTable
getValueTable  = Parse (psValueTable <$> get)

-- | Retrieve the name for the next value.  Note that this doesn't assume that
-- the name gets used, and doesn't update the next id in the value table.
getNextId :: Parse Int
getNextId  = valueNextId <$> getValueTable

-- | Set the current value table.
setValueTable :: ValueTable -> Parse ()
setValueTable vt = Parse $ do
  ps <- get
  set ps { psValueTable = vt }

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

getMdTable :: Parse MdTable
getMdTable  = Parse (psMdTable <$> get)

setMdTable :: MdTable -> Parse ()
setMdTable md = Parse $ do
  ps <- get
  set $! ps { psMdTable = md }

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
  nodeRef   = reference `fmap` Map.lookup ix (psMdRefs ps)
  mdValue   = Map.lookup ix (valueEntries (psMdTable ps))


type MdRefTable = Map.Map Int Int

setMdRefs :: MdRefTable -> Parse ()
setMdRefs refs = Parse $ do
  ps <- get
  set $! ps { psMdRefs = refs `Map.union` psMdRefs ps }


-- Function Prototypes ---------------------------------------------------------

data FunProto = FunProto
  { protoType  :: Type
  , protoAttrs :: FunAttrs
  , protoName  :: String
  , protoIndex :: Int
  } deriving (Show)

-- | Push a function prototype on to the prototype stack.
pushFunProto :: FunProto -> Parse ()
pushFunProto p = Parse $ do
  ps <- get
  set ps { psFunProtos = psFunProtos ps Seq.|> p }

-- | Take a single function prototype off of the prototype stack.
popFunProto :: Parse FunProto
popFunProto  = do
  ps <- Parse get
  case Seq.viewl (psFunProtos ps) of
    Seq.EmptyL   -> fail "empty function prototype stack"
    p Seq.:< ps' -> do
      Parse (set ps { psFunProtos = ps' })
      return p


-- Parsing Environment ---------------------------------------------------------

data Env = Env
  { envSymtab  :: Symtab
  , envContext :: [String]
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

getContext :: Parse [String]
getContext  = Parse (envContext `fmap` ask)


data Symtab = Symtab
  { symValueSymtab :: ValueSymtab
  , symTypeSymtab  :: TypeSymtab
  } deriving (Show)

instance Monoid Symtab where
  mempty = Symtab
    { symValueSymtab = emptyValueSymtab
    , symTypeSymtab  = mempty
    }

  mappend l r = Symtab
    { symValueSymtab = symValueSymtab l `Map.union` symValueSymtab r
    , symTypeSymtab  = symTypeSymtab  l `mappend`   symTypeSymtab  r
    }

withSymtab :: Symtab -> Parse a -> Parse a
withSymtab symtab body = Parse $ do
  env <- ask
  local (extendSymtab symtab env) (unParse body)

-- | Run a computation with an extended value symbol table.
withValueSymtab :: ValueSymtab -> Parse a -> Parse a
withValueSymtab symtab = withSymtab (mempty { symValueSymtab = symtab })

-- | Retrieve the value symbol table.
getValueSymtab :: Parse ValueSymtab
getValueSymtab  = Parse (symValueSymtab . envSymtab <$> ask)

-- | Run a computation with an extended type symbol table.
withTypeSymtab :: TypeSymtab -> Parse a -> Parse a
withTypeSymtab symtab = withSymtab (mempty { symTypeSymtab = symtab })

-- | Retrieve the type symbol table.
getTypeSymtab :: Parse TypeSymtab
getTypeSymtab  = Parse (symTypeSymtab . envSymtab <$> ask)

-- | Label a sub-computation with its context.
label :: String -> Parse a -> Parse a
label l m = Parse $ do
  env <- ask
  local (addLabel l env) (unParse m)

-- | Fail, taking into account the current context.
failWithContext :: String -> Parse a
failWithContext msg = Parse $ do
  env <- ask
  raise Error
    { errMessage = msg
    , errContext = envContext env
    }

-- | Attempt to find the type id in the type symbol table, when that fails,
-- look it up in the type table.
getType :: Int -> Parse Type
getType ref = do
  symtab <- getTypeSymtab
  case Map.lookup ref (tsById symtab) of
    Just i  -> return (Alias i)
    Nothing -> getType' ref

-- | Find the id associated with a type alias.
getTypeId :: Ident -> Parse Int
getTypeId n = do
  symtab <- getTypeSymtab
  case Map.lookup n (tsByName symtab) of
    Just ix -> return ix
    Nothing -> fail ("unknown type alias " ++ show (ppIdent n))


-- Value Symbol Table ----------------------------------------------------------

type SymName = Either String Int

type ValueSymtab = Map.Map SymTabEntry SymName

data SymTabEntry
  = SymTabEntry !Int
  | SymTabBBEntry !Int
    deriving (Eq,Ord,Show)

renderName :: SymName -> String
renderName  = either id show

mkBlockLabel :: SymName -> BlockLabel
mkBlockLabel  = either (Named . Ident) Anon

emptyValueSymtab :: ValueSymtab
emptyValueSymtab  = Map.empty

addEntry :: Int -> String -> ValueSymtab -> ValueSymtab
addEntry i n = Map.insert (SymTabEntry i) (Left n)

addBBEntry :: Int -> String -> ValueSymtab -> ValueSymtab
addBBEntry i n = Map.insert (SymTabBBEntry i) (Left n)

addBBAnon :: Int -> Int -> ValueSymtab -> ValueSymtab
addBBAnon i n = Map.insert (SymTabBBEntry i) (Right n)

-- | Lookup the name of an entry.
entryName :: Int -> Parse String
entryName n = do
  symtab <- getValueSymtab
  case Map.lookup (SymTabEntry n) symtab of
    Just i  -> return (renderName i)
    Nothing -> fail ("entry " ++ show n ++ " is missing from the symbol table"
             ++ "\n" ++ show symtab)

-- | Lookup the name of a basic block.
bbEntryName :: Int -> Parse (Maybe BlockLabel)
bbEntryName n = do
  symtab <- getValueSymtab
  return (mkBlockLabel <$> Map.lookup (SymTabBBEntry n) symtab)

-- | Lookup the name of a basic block.
requireBbEntryName :: Int -> Parse BlockLabel
requireBbEntryName n = do
  mb <- bbEntryName n
  case mb of
    Just l  -> return l
    Nothing -> fail ("basic block " ++ show n ++ " has no id")


-- Type Symbol Tables ----------------------------------------------------------

data TypeSymtab = TypeSymtab
  { tsById   :: Map.Map Int Ident
  , tsByName :: Map.Map Ident Int
  } deriving Show

instance Monoid TypeSymtab where
  mempty = TypeSymtab
    { tsById   = Map.empty
    , tsByName = Map.empty
    }

  mappend l r = TypeSymtab
    { tsById   = tsById   l `Map.union` tsById r
    , tsByName = tsByName l `Map.union` tsByName r
    }

addTypeSymbol :: Int -> Ident -> TypeSymtab -> TypeSymtab
addTypeSymbol ix n ts = ts
  { tsById   = Map.insert ix n (tsById ts)
  , tsByName = Map.insert n ix (tsByName ts)
  }
