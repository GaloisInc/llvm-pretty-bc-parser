{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Data.LLVM.BitCode.IR.Metadata (
    parseMetadataBlock
  , PartialUnnamedMd(..)
  , finalizePartialUnnamedMd
  , MetadataAttachments
  ) where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST
import Text.LLVM.Labels

import Control.Exception (throw)
import Control.Monad (foldM,guard,mplus,unless,when)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Traversable as T


-- Parsing State ---------------------------------------------------------------

data MetadataTable = MetadataTable
  { mtEntries   :: MdTable
  , mtNextNode  :: !Int
  , mtNodes     :: Map.Map Int (Bool,Bool,Int)
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

addMdValue :: Typed PValue -> MetadataTable -> MetadataTable
addMdValue tv mt = mt { mtEntries = addValue tv (mtEntries mt) }

nameNode :: Bool -> Bool -> Int -> MetadataTable -> MetadataTable
nameNode fnLocal isDistinct ix mt = mt
  { mtNodes    = Map.insert ix (fnLocal,isDistinct,mtNextNode mt) (mtNodes mt)
  , mtNextNode = mtNextNode mt + 1
  }

addString :: String -> MetadataTable -> MetadataTable
addString str = snd . addMetadata (ValMdString str)

-- | Add a new node, that might be distinct.
addNode :: Bool -> [Typed PValue] -> MetadataTable -> MetadataTable
addNode isDistinct vals mt = nameNode False isDistinct ix mt'
  where
  (ix,mt') = addMetadata (ValMdNode vals) mt

addOldNode :: Bool -> [Typed PValue] -> MetadataTable -> MetadataTable
addOldNode fnLocal vals mt = nameNode fnLocal False ix mt'
  where
  (ix,mt') = addMetadata (ValMdNode vals) mt

mdForwardRef :: [String] -> MetadataTable -> Int -> Typed PValue
mdForwardRef cxt mt ix = fromMaybe fallback nodeRef
  where
  fallback          = forwardRef cxt ix (mtEntries mt)
  reference (_,_,r) = metadata (ValMdRef r)
  nodeRef           = reference `fmap` Map.lookup ix (mtNodes mt)

mdNodeRef :: [String] -> MetadataTable -> Int -> Int
mdNodeRef cxt mt ix =
  maybe (throw (BadValueRef cxt ix)) prj (Map.lookup ix (mtNodes mt))
  where
  prj (_,_,x) = x

mkMdRefTable :: MetadataTable -> MdRefTable
mkMdRefTable mt = Map.mapMaybe step (mtNodes mt)
  where
  step (fnLocal,_,ix) = do
    guard (not fnLocal)
    return ix

data PartialMetadata = PartialMetadata
  { pmEntries       :: MetadataTable
  , pmNamedEntries  :: Map.Map String [Int]
  , pmNextName      :: Maybe String
  , pmAttachments   :: MetadataAttachments
  } deriving (Show)

emptyPartialMetadata :: MdTable -> PartialMetadata
emptyPartialMetadata es = PartialMetadata
  { pmEntries       = emptyMetadataTable es
  , pmNamedEntries  = Map.empty
  , pmNextName      = Nothing
  , pmAttachments   = Map.empty
  }

updateMetadataTable :: (MetadataTable -> MetadataTable)
                    -> (PartialMetadata -> PartialMetadata)
updateMetadataTable f pm = pm { pmEntries = f (pmEntries pm) }

setNextName :: String -> PartialMetadata -> PartialMetadata
setNextName name pm = pm { pmNextName = Just name }

addAttachment :: Int -> [(String,PValMd)] -> PartialMetadata -> PartialMetadata
addAttachment instr md pm =
  pm { pmAttachments = Map.insert instr md (pmAttachments pm) }

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
  , pumDistinct :: Bool
  } deriving (Show)

finalizePartialUnnamedMd :: PartialUnnamedMd -> Parse UnnamedMd
finalizePartialUnnamedMd pum = mkUnnamedMd `fmap` fixLabels (pumValues pum)
  where
  -- map through the list and typed PValue to change labels to textual ones
  fixLabels      = mapM (T.mapM (relabel (const requireBbEntryName)))
  mkUnnamedMd vs = UnnamedMd
    { umIndex  = pumIndex pum
    , umValues = vs
    , umDistinct = pumDistinct pum
    }

unnamedEntries :: PartialMetadata -> ([PartialUnnamedMd],[PartialUnnamedMd])
unnamedEntries pm = foldl resolveNode ([],[]) (Map.toList (mtNodes mt))
  where
  mt = pmEntries pm
  es = valueEntries (mtEntries mt)

  resolveNode (gs,fs) (ref,(fnLocal,d,ix)) = case lookupNode ref d ix of
    Just pum | fnLocal   -> (gs,pum:fs)
             | otherwise -> (pum:gs,fs)
    Nothing              -> (gs,fs)

  lookupNode ref d ix = do
    Typed { typedValue = ValMd (ValMdNode vs) } <- Map.lookup ref es
    return PartialUnnamedMd
      { pumIndex  = ix
      , pumValues = vs
      , pumDistinct = d
      }

type MetadataAttachments = Map.Map Int [(String,PValMd)]
type ParsedMetadata = ([NamedMd],([PartialUnnamedMd],[PartialUnnamedMd]),MetadataAttachments)

parsedMetadata :: PartialMetadata -> ParsedMetadata
parsedMetadata pm = (namedEntries pm, unnamedEntries pm, pmAttachments pm)

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
    str <- parseFields r 0 char `mplus` parseField r 0 cstring
    return $! updateMetadataTable (addString str) pm

  -- [type num, value num]
  2 -> label "METADATA_VALUE" $ do
    unless (length (recordFields r) == 2)
           (fail "Invalid record")

    let field = parseField r
    ty  <- getType =<< field 0 numeric
    when (ty == PrimType Metadata || ty == PrimType Void)
         (fail "invalid record")

    cxt <- getContext
    ix  <- field 1 numeric
    let tv = forwardRef cxt ix vt

    return $! updateMetadataTable (addMdValue tv) pm


  -- [n x md num]
  3 -> label "METADATA_NODE" (parseMetadataNode False mt r pm)

  -- [values]
  4 -> label "METADATA_NAME" $ do
    name <- parseFields r 0 char
    return $! setNextName name pm

  -- [n x md num]
  5 -> label "METADATA_DISTINCT_NODE" (parseMetadataNode True mt r pm)

  -- [n x [id, name]]
  6 -> label "METADATA_KIND" $ do
    kind <- parseField  r 0 numeric
    name <- parseFields r 1 char
    addKind kind name
    return pm

  -- [distinct, line, col, scope, inlined-at?] 
  7 -> label "METADATA_LOCATION" $ do
    fail "not implemented"

  -- [n x (type num, value num)]
  8 -> label "METADATA_OLD_NODE" (parseMetadataOldNode False vt mt r pm)

  -- [n x (type num, value num)]
  9 -> label "METADATA_OLD_FN_NODE" (parseMetadataOldNode True vt mt r pm)

  -- [n x mdnodes]
  10 -> label "METADATA_NAMED_NODE" $ do
    mdIds <- parseFields r 0 numeric
    cxt   <- getContext
    let ids = map (mdNodeRef cxt mt) mdIds
    nameMetadata ids pm

  -- [m x [value, [n x [id, mdnode]]]
  11 -> label "METADATA_ATTACHMENT" $ do
    let field = parseField r
    inst <- field 0 numeric
    md   <- parseAttachment r
    return $! addAttachment inst md pm

  code -> fail ("unknown record code: " ++ show code)

parseMetadataEntry _ _ pm (abbrevDef -> Just _) =
  return pm

parseMetadataEntry _ _ _ r =
  fail ("unexpected: " ++ show r)


parseAttachment :: Record -> Parse [(String,PValMd)]
parseAttachment r = loop (length (recordFields r) - 1) []
  where
  loop 0 acc = return acc
  loop n acc = do
    kind <- getKind     =<< parseField r (n - 1) numeric
    md   <- getMetadata =<< parseField r  n      numeric
    loop (n - 2) ((kind,typedValue md) : acc)

-- | Parse a metadata node.
parseMetadataNode :: Bool -> MetadataTable -> Record -> PartialMetadata
                  -> Parse PartialMetadata
parseMetadataNode isDistinct mt r pm = do
  ixs <- parseFields r 0 numeric
  cxt <- getContext
  let tvs = [ mdForwardRef cxt mt (ix - 1) | ix <- ixs ]
  return $! updateMetadataTable (addNode isDistinct tvs) pm


-- | Parse out a metadata node in the old format.
parseMetadataOldNode :: Bool -> ValueTable -> MetadataTable -> Record
                     -> PartialMetadata -> Parse PartialMetadata
parseMetadataOldNode fnLocal vt mt r pm = do
  values <- loop =<< parseFields r 0 numeric
  return $! updateMetadataTable (addOldNode fnLocal values) pm
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
