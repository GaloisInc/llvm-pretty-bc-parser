{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Data.LLVM.BitCode.IR.Metadata (
    parseMetadataBlock
  , parseMetadataKindEntry
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

addLoc :: Bool -> PDebugLoc -> MetadataTable -> MetadataTable
addLoc isDistinct loc mt = nameNode False isDistinct ix mt'
  where
  (ix,mt') = addMetadata (ValMdLoc loc) mt

-- | Add a new node, that might be distinct.
addNode :: Bool -> [Maybe PValMd] -> MetadataTable -> MetadataTable
addNode isDistinct vals mt = nameNode False isDistinct ix mt'
  where
  (ix,mt') = addMetadata (ValMdNode vals) mt

addOldNode :: Bool -> [Typed PValue] -> MetadataTable -> MetadataTable
addOldNode fnLocal vals mt = nameNode fnLocal False ix mt'
  where
  (ix,mt') = addMetadata (ValMdNode [ Just (ValMdValue tv) | tv <- vals ]) mt

mdForwardRef :: [String] -> MetadataTable -> Int -> PValMd
mdForwardRef cxt mt ix = fromMaybe fallback nodeRef
  where
  fallback          = case forwardRef cxt ix (mtEntries mt) of
                        Typed { typedValue = ValMd md } -> md
                        tv                              -> ValMdValue tv
  reference (_,_,r) = ValMdRef r
  nodeRef           = reference `fmap` Map.lookup ix (mtNodes mt)

mdForwardRefOrNull :: [String] -> MetadataTable -> Int -> Maybe PValMd
mdForwardRefOrNull cxt mt ix | ix > 0 = Just (mdForwardRef cxt mt (ix - 1))
                             | otherwise = Nothing

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
  , pumValues :: [Maybe PValMd]
  , pumDistinct :: Bool
  } deriving (Show)

finalizePartialUnnamedMd :: PartialUnnamedMd -> Parse UnnamedMd
finalizePartialUnnamedMd pum = mkUnnamedMd `fmap` fixLabels (pumValues pum)
  where
  -- map through the list and typed PValue to change labels to textual ones
  fixLabels      = T.mapM (T.mapM (relabel (const requireBbEntryName)))
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
    str <- parseFields r 0 char `mplus` parseField r 0 string
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
    name <- parseFields r 0 char `mplus` parseField r 0 cstring
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
    cxt       <- getContext
    let field = parseField r
    isDistinct <- field 0 nonzero
    dlLine     <- field 1 numeric
    dlCol      <- field 2 numeric
    dlScope    <- mdForwardRef cxt mt <$> field 3 numeric
    dlIA       <- mdForwardRefOrNull cxt mt <$> field 4 numeric
    let loc = DebugLoc { .. }
    return $! updateMetadataTable (addLoc isDistinct loc) pm


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

  12 -> label "METADATA_GENERIC_DEBUG" $ do
    isDistinct <- parseField r 0 numeric
    tag <- parseField r 1 numeric
    version <- parseField r 2 numeric
    header <- parseField r 3 string
    -- TODO: parse all remaining fields
    fail "not yet implemented"
  13 -> label "METADATA_SUBRANGE" $ do
    isDistinct <- parseField r 0 numeric
    parseField r 1 numeric
    parseField r 2 signed
    -- TODO
    fail "not yet implemented"
  14 -> label "METADATA_ENUMERATOR" $ do
    isDistinct <- parseField r 0 numeric
    parseField r 1 signed
    parseField r 2 string
    -- TODO
    fail "not yet implemented"
  15 -> label "METADATA_BASIC_TYPE" $ do
    isDistinct <- parseField r 0 numeric
    parseField r 1 numeric
    name <- parseField r 2 numeric
    parseField r 3 numeric
    parseField r 4 numeric
    parseField r 5 numeric
    -- TODO
    fail "not yet implemented"
  16 -> label "METADATA_FILE" $ do
    isDistinct <- parseField r 0 numeric
    name <- parseField r 1 numeric
    dir  <- parseField r 2 numeric
    -- TODO
    fail "not yet implemented"
  17 -> label "METADATA_DERIVED_TYPE" $ do
    -- TODO
    fail "not yet implemented"
  18 -> label "METADATA_COMPOSITE_TYPE" $ do
    -- TODO
    fail "not yet implemented"
  19 -> label "METADATA_SUBROUTINE_TYPE" $ do
    -- TODO
    fail "not yet implemented"
  20 -> label "METADATA_COMPILE_UNIT" $ do
    -- TODO
    fail "not yet implemented"
  21 -> label "METADATA_SUBPROGRAM" $ do
    -- TODO
    fail "not yet implemented"
  22 -> label "METADATA_LEXICAL_BLOCK" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 2 numeric
    parseField r 3 numeric
    parseField r 4 numeric
    -- TODO
    fail "not yet implemented"
  23 -> label "METADATA_LEXICAL_BLOCK_FILE" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 2 numeric
    parseField r 3 numeric
    -- TODO
    fail "not yet implemented"
  24 -> label "METADATA_NAMESPACE" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 2 numeric
    parseField r 3 string
    parseField r 4 numeric
    -- TODO
    fail "not yet implemented"
  25 -> label "METADATA_TEMPLATE_TYPE" $ do
    isDistinct <- parseField r 0 numeric
    parseField r 1 string
    -- getDITypeRefOrNull <$> parseField r 2 numeric
    -- TODO
    fail "not yet implemented"
  26 -> label "METADATA_TEMPLATE_VALUE" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    parseField r 1 numeric
    parseField r 2 string
    -- getDITypeRefOrNull cxt mt <$> parseField r 3 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 4 numeric
    -- TODO
    fail "not yet implemented"
  27 -> label "METADATA_GLOBAL_VAR" $ do
    -- TODO
    fail "not yet implemented"
  28 -> label "METADATA_LOCAL_VAR" $ do
    -- TODO
    fail "not yet implemented"
  29 -> label "METADATA_EXPRESSION" $ do
    -- TODO
    fail "not yet implemented"
  30 -> label "METADATA_OBJC_PROPERTY" $ do
    -- TODO
    fail "not yet implemented"
  31 -> label "METADATA_IMPORTED_ENTITY" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    parseField r 1 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 2 numeric
    -- getDITypeRefOrNull cxt mt <$> parseField r 3 numeric
    parseField r 4 numeric
    parseField r 5 string
    -- TODO
    fail "not yet implemented"
  32 -> label "METADATA_MODULE" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
    parseField r 2 string
    parseField r 3 string
    parseField r 4 string
    parseField r 5 string
    -- TODO
    fail "not yet implemented"
  33 -> label "METADATA_MACRO" $ do
    isDistinct <- parseField r 0 numeric
    parseField r 1 numeric
    parseField r 2 numeric
    parseField r 3 string
    parseField r 4 string
    -- TODO
    fail "not yet implemented"
  34 -> label "METADATA_MACRO_FILE" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 numeric
    parseField r 1 numeric
    parseField r 2 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 3 numeric
    mdForwardRefOrNull cxt mt <$> parseField r 4 numeric
    -- TODO
    fail "not yet implemented"
  35 -> label "METADATA_STRINGS" $ do
    -- TODO
    fail "not yet implemented"
  36 -> label "METADATA_GLOBAL_DECL_ATTACHMENT" $ do
    -- TODO
    fail "not yet implemented"

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
  let lkp = mdForwardRefOrNull cxt mt
  return $! updateMetadataTable (addNode isDistinct (map lkp ixs)) pm


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
        PrimType Metadata -> return $ Typed (PrimType Metadata)
                                            (ValMd (mdForwardRef cxt mt valId))
        -- XXX need to check for a void type here
        _                 -> return (forwardRef cxt valId vt)

      vals <- loop rest
      return (val:vals)

    [] -> return []

    _ -> fail "Malformed metadata node"

parseMetadataKindEntry :: Record -> Parse ()
parseMetadataKindEntry r = do
  kind <- parseField  r 0 numeric
  name <- parseFields r 1 char
  addKind kind name
