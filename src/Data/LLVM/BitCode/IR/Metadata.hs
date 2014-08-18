{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Data.LLVM.BitCode.IR.Metadata (
    parseMetadataBlock
  , PartialUnnamedMd(..)
  , finalizePartialUnnamedMd
  ) where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST
import Text.LLVM.Labels

import Control.Exception (throw)
import Control.Monad (foldM,guard)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Traversable as T


-- Kind Tables -----------------------------------------------------------------

data KindTable = KindTable
  { ktNextId  :: !Int
  , ktNameMap :: Map.Map String Int
  } deriving (Show)

emptyKindTable :: KindTable
emptyKindTable  = KindTable
  { ktNextId  = 5
  , ktNameMap = Map.fromList
    [ ("dbg",    0)
    , ("tbaa",   1)
    , ("prof",   2)
    , ("fpmath", 3)
    , ("range",  4)
    ]
  }

getKindId :: String -> KindTable -> (Int,KindTable)
getKindId str kt = case Map.lookup str (ktNameMap kt) of
  Just i  -> (i,kt)
  Nothing -> (ktNextId kt, kt')
  where
  kt' = kt
    { ktNextId  = ktNextId kt + 1
    , ktNameMap = Map.insert str (ktNextId kt) (ktNameMap kt)
    }


-- Parsing State ---------------------------------------------------------------

data MetadataTable = MetadataTable
  { mtEntries   :: MdTable
  , mtNextNode  :: !Int
  , mtNodes     :: Map.Map Int (Bool,Int)
  } deriving (Show)

emptyMetadataTable :: MdTable -> MetadataTable
emptyMetadataTable es = MetadataTable
  { mtEntries   = es
  , mtNextNode  = 0
  , mtNodes     = Map.empty
  }

metadata :: PValMd -> Typed PValue
metadata  = Typed (PrimType Metadata) . ValMd

addMetadata :: PValMd  -> MetadataTable -> (Int,MetadataTable)
addMetadata val mt = (ix, mt { mtEntries = es' })
  where
  (ix,es') = addValue' (metadata val) (mtEntries mt)

nameNode :: Bool -> Int -> MetadataTable -> MetadataTable
nameNode fnLocal ix mt = mt
  { mtNodes    = Map.insert ix (fnLocal,mtNextNode mt) (mtNodes mt)
  , mtNextNode = mtNextNode mt + 1
  }

addString :: String -> MetadataTable -> MetadataTable
addString str = snd . addMetadata (ValMdString str)

addNode :: Bool -> [Typed PValue] -> MetadataTable -> MetadataTable
addNode fnLocal vals mt = nameNode fnLocal ix mt'
  where
  (ix,mt') = addMetadata (ValMdNode vals) mt

mdForwardRef :: [String] -> MetadataTable -> Int -> Typed PValue
mdForwardRef cxt mt ix = fromMaybe fallback nodeRef
  where
  fallback  = forwardRef cxt ix (mtEntries mt)
  reference = metadata . ValMdRef . snd
  nodeRef   = reference `fmap` Map.lookup ix (mtNodes mt)

mdNodeRef :: [String] -> MetadataTable -> Int -> Int
mdNodeRef cxt mt ix =
  maybe (throw (BadValueRef cxt ix)) snd (Map.lookup ix (mtNodes mt))

mkMdRefTable :: MetadataTable -> MdRefTable
mkMdRefTable mt = Map.mapMaybe step (mtNodes mt)
  where
  step (fnLocal,ix) = do
    guard (not fnLocal)
    return ix

type KindMap = Map.Map Int Int

data PartialMetadata = PartialMetadata
  { pmEntries       :: MetadataTable
  , pmNamedEntries  :: Map.Map String [Int]
  , pmKindMap       :: KindMap
  , pmKindTable     :: KindTable
  , pmNextName      :: Maybe String
  } deriving (Show)

emptyPartialMetadata :: MdTable -> PartialMetadata
emptyPartialMetadata es = PartialMetadata
  { pmEntries       = emptyMetadataTable es
  , pmNamedEntries  = Map.empty
  , pmKindMap       = Map.empty
  , pmKindTable     = emptyKindTable
  , pmNextName      = Nothing
  }

updateMetadataTable :: (MetadataTable -> MetadataTable)
                    -> (PartialMetadata -> PartialMetadata)
updateMetadataTable f pm = pm { pmEntries = f (pmEntries pm) }

addKind :: Int -> String -> PartialMetadata -> PartialMetadata
addKind ix str pm = pm
  { pmKindMap   = Map.insert ix strId (pmKindMap pm)
  , pmKindTable = kt'
  }
  where
  (strId,kt') = getKindId str (pmKindTable pm)

setNextName :: String -> PartialMetadata -> PartialMetadata
setNextName name pm = pm { pmNextName = Just name }

nameMetadata :: [Int] -> PartialMetadata -> Parse PartialMetadata
nameMetadata val pm = case pmNextName pm of
  Just name -> return $! pm
    { pmNextName     = Nothing
    , pmNamedEntries = Map.insert name val (pmNamedEntries pm)
    }
  Nothing -> fail "Expected a metadata name"

namedEntries :: PartialMetadata -> [NamedMd]
namedEntries  = map (uncurry NamedMd)
              . Map.toList
              . pmNamedEntries

data PartialUnnamedMd = PartialUnnamedMd
  { pumIndex  :: Int
  , pumValues :: [Typed PValue]
  } deriving (Show)

finalizePartialUnnamedMd :: PartialUnnamedMd -> Parse UnnamedMd
finalizePartialUnnamedMd pum = mkUnnamedMd `fmap` fixLabels (pumValues pum)
  where
  -- map through the list and typed PValue to change labels to textual ones
  fixLabels      = mapM (T.mapM (relabel (const requireBbEntryName)))
  mkUnnamedMd vs = UnnamedMd
    { umIndex  = pumIndex pum
    , umValues = vs
    }

unnamedEntries :: PartialMetadata -> ([PartialUnnamedMd],[PartialUnnamedMd])
unnamedEntries pm = foldl resolveNode ([],[]) (Map.toList (mtNodes mt))
  where
  mt = pmEntries pm
  es = valueEntries (mtEntries mt)

  resolveNode (gs,fs) (ref,(fnLocal,ix)) = case lookupNode ref ix of
    Just pum | fnLocal   -> (gs,pum:fs)
             | otherwise -> (pum:gs,fs)
    Nothing              -> (gs,fs)

  lookupNode ref ix = do
    Typed { typedValue = ValMd (ValMdNode vs) } <- Map.lookup ref es
    return PartialUnnamedMd
      { pumIndex  = ix
      , pumValues = vs
      }

type ParsedMetadata = ([NamedMd],([PartialUnnamedMd],[PartialUnnamedMd]))

parsedMetadata :: PartialMetadata -> ParsedMetadata
parsedMetadata pm = (namedEntries pm, unnamedEntries pm)

-- Metadata Parsing ------------------------------------------------------------

parseMetadataBlock :: ValueTable -> [Entry] -> Parse ParsedMetadata
parseMetadataBlock vt es = label "METADATA_BLOCK" $ do
  ms <- getMdTable
  let pm0 = emptyPartialMetadata ms
  rec pm <- foldM (parseMetadataEntry vt (pmEntries pm)) pm0 es
  let entries = pmEntries pm
  setMdTable (mtEntries entries)
  setMdRefs  (mkMdRefTable entries)
  return (parsedMetadata pm)

-- | Parse an entry in the metadata block.
--
-- XXX this currently relies on the constant block having been parsed already.
-- Though most bitcode examples I've seen are ordered this way, it would be nice
-- to not have to rely on it.
parseMetadataEntry :: ValueTable -> MetadataTable -> PartialMetadata -> Entry
                   -> Parse PartialMetadata
parseMetadataEntry vt mt pm (fromEntry -> Just r) = case recordCode r of
  -- [values]
  1 -> label "METADATA_STRING" $ do
    str <- parseFields r 0 char
    return $! updateMetadataTable (addString str) pm

  -- 2 is unused.

  -- 3 is unused.

  -- [values]
  4 -> label "METADATA_NAME" $ do
    name <- parseFields r 0 char
    return $! setNextName name pm

  -- 5 is unused.

  -- [n x [id, name]]
  6 -> label "METADATA_KIND" $ do
    kind <- parseField  r 0 numeric
    name <- parseFields r 1 char
    return $! addKind kind name pm

  -- 7 is unused.

  -- [n x (type num, value num)]
  8 -> label "METADATA_NODE" (parseMetadataNode False vt mt r pm)

  -- [n x (type num, value num)]
  9 -> label "METADATA_FN_NODE" (parseMetadataNode True vt mt r pm)

  -- [n x mdnodes]
  10 -> label "METADATA_NAMED_NODE" $ do
    mdIds <- parseFields r 0 numeric
    cxt   <- getContext
    let ids = map (mdNodeRef cxt mt) mdIds
    nameMetadata ids pm

  -- [m x [value, [n x [id, mdnode]]]
  11 -> label "METADATA_ATTACHMENT" $ do
    fail "not implemented"

  code -> fail ("unknown record code: " ++ show code)

parseMetadataEntry _ _ pm (abbrevDef -> Just _) =
  return pm

parseMetadataEntry _ _ _ r =
  fail ("unexpected: " ++ show r)


-- | Parse out a metadata node.
parseMetadataNode :: Bool -> ValueTable -> MetadataTable -> Record
                  -> PartialMetadata -> Parse PartialMetadata
parseMetadataNode fnLocal vt mt r pm = do
  values <- loop =<< parseFields r 0 numeric
  return $! updateMetadataTable (addNode fnLocal values) pm
  where
  loop fs = case fs of

    tyId:valId:rest -> do
      cxt <- getContext
      ty  <- getType' tyId
      val <- case ty of
        PrimType Metadata -> return (mdForwardRef cxt mt valId)
        -- XXX need to check for a void type here
        _                 -> return (forwardRef cxt valId vt)

      vals <- loop rest
      return (val:vals)

    [] -> return []

    _ -> fail "Malformed metadata node"
