{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}

module Data.LLVM.BitCode.IR.Metadata (
    parseMetadataBlock
  , parseMetadataKindEntry
  , PartialUnnamedMd(..)
  , finalizePartialUnnamedMd
  , finalizePValMd
  , InstrMdAttachments
  , PFnMdAttachments
  , PKindMd
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

addDebugInfo
  :: Bool
  -> DebugInfo' Int
  -> MetadataTable
  -> MetadataTable
addDebugInfo isDistinct di mt = nameNode False isDistinct ix mt'
  where
  (ix,mt') = addMetadata (ValMdDebugInfo di) mt

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

mdString :: [String] -> MetadataTable -> Int -> String
mdString cxt mt ix =
  fromMaybe (throw (BadValueRef cxt ix)) (mdStringOrNull cxt mt ix)

mdStringOrNull :: [String] -> MetadataTable -> Int -> Maybe String
mdStringOrNull cxt mt ix =
  case mdForwardRefOrNull cxt mt ix of
    Nothing -> Nothing
    Just (ValMdString str) -> Just str
    Just _ -> throw (BadTypeRef cxt ix)

mkMdRefTable :: MetadataTable -> MdRefTable
mkMdRefTable mt = Map.mapMaybe step (mtNodes mt)
  where
  step (fnLocal,_,ix) = do
    guard (not fnLocal)
    return ix

data PartialMetadata = PartialMetadata
  { pmEntries          :: MetadataTable
  , pmNamedEntries     :: Map.Map String [Int]
  , pmNextName         :: Maybe String
  , pmInstrAttachments :: InstrMdAttachments
  , pmFnAttachments    :: PFnMdAttachments
  } deriving (Show)

emptyPartialMetadata :: MdTable -> PartialMetadata
emptyPartialMetadata es = PartialMetadata
  { pmEntries          = emptyMetadataTable es
  , pmNamedEntries     = Map.empty
  , pmNextName         = Nothing
  , pmInstrAttachments = Map.empty
  , pmFnAttachments    = Map.empty
  }

updateMetadataTable :: (MetadataTable -> MetadataTable)
                    -> (PartialMetadata -> PartialMetadata)
updateMetadataTable f pm = pm { pmEntries = f (pmEntries pm) }

setNextName :: String -> PartialMetadata -> PartialMetadata
setNextName name pm = pm { pmNextName = Just name }

addFnAttachment :: PFnMdAttachments -> PartialMetadata -> PartialMetadata
addFnAttachment att pm =
  -- left-biased union, since the parser overwrites metadata as it encounters it
  pm { pmFnAttachments = Map.union att (pmFnAttachments pm) }

addInstrAttachment :: Int -> [(KindMd,PValMd)]
                   -> PartialMetadata -> PartialMetadata
addInstrAttachment instr md pm =
  pm { pmInstrAttachments = Map.insert instr md (pmInstrAttachments pm) }

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
  fixLabels      = T.mapM (T.mapM finalizePValMd)
  mkUnnamedMd vs = UnnamedMd
    { umIndex  = pumIndex pum
    , umValues = vs
    , umDistinct = pumDistinct pum
    }

finalizePValMd :: PValMd -> Parse ValMd
finalizePValMd = relabel (const requireBbEntryName)

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

type InstrMdAttachments = Map.Map Int [(KindMd,PValMd)]

type PKindMd = Int
type PFnMdAttachments = Map.Map PKindMd PValMd

type ParsedMetadata =
  ( [NamedMd]
  , ([PartialUnnamedMd],[PartialUnnamedMd])
  , InstrMdAttachments
  , PFnMdAttachments
  )

parsedMetadata :: PartialMetadata -> ParsedMetadata
parsedMetadata pm =
  ( namedEntries pm
  , unnamedEntries pm
  , pmInstrAttachments pm
  , pmFnAttachments pm
  )

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
    let recordSize = length (recordFields r)
    when (recordSize == 0)
      (fail "Invalid record")
    ctx <- getContext
    if (recordSize `mod` 2 == 0)
      then label "function attachment" $ do
        att <- Map.fromList <$> parseAttachment r 0
        return $! addFnAttachment att pm
      else label "instruction attachment" $ do
        inst <- parseField r 0 numeric
        patt <- parseAttachment r 1
        att <- mapM (\(k,md) -> (,md) <$> getKind k) patt
        return $! addInstrAttachment inst att pm

  12 -> label "METADATA_GENERIC_DEBUG" $ do
    isDistinct <- parseField r 0 numeric
    tag <- parseField r 1 numeric
    version <- parseField r 2 numeric
    header <- parseField r 3 string
    -- TODO: parse all remaining fields
    fail "not yet implemented"

  13 -> label "METADATA_SUBRANGE" $ do
    isDistinct <- parseField r 0 nonzero
    disrCount <- parseField r 1 numeric
    disrLowerBound <- parseField r 2 signedInt64
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoSubrange DISubrange{..})) pm

  14 -> label "METADATA_ENUMERATOR" $ do
    isDistinct <- parseField r 0 numeric
    parseField r 1 signedInt64
    parseField r 2 string
    -- TODO
    fail "not yet implemented"

  15 -> label "METADATA_BASIC_TYPE" $ do
    ctx <- getContext
    isDistinct <- parseField r 0 nonzero
    dibtTag <- parseField r 1 numeric
    dibtName <- mdString ctx mt <$> parseField r 2 numeric
    dibtSize <- parseField r 3 numeric
    dibtAlign <- parseField r 4 numeric
    dibtEncoding <- parseField r 5 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoBasicType DIBasicType{..})) pm

  -- [distinct, filename, directory]
  16 -> label "METADATA_FILE" $ do
    ctx <- getContext
    isDistinct <- parseField r 0 nonzero
    difFilename <- mdString ctx mt <$> parseField r 1 numeric
    difDirectory <- mdString ctx mt <$> parseField r 2 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoFile DIFile{..})) pm

  17 -> label "METADATA_DERIVED_TYPE" $ do
    ctx <- getContext
    isDistinct    <- parseField r 0 nonzero
    didtTag       <- parseField r 1 numeric
    didtName      <- mdStringOrNull     ctx mt <$> parseField r 2 numeric
    didtFile      <- mdForwardRefOrNull ctx mt <$> parseField r 3 numeric
    didtLine      <- parseField r 4 numeric
    didtScope     <- mdForwardRefOrNull ctx mt <$> parseField r 5 numeric
    didtBaseType  <- mdForwardRefOrNull ctx mt <$> parseField r 6 numeric
    didtSize      <- parseField r 7 numeric
    didtAlign     <- parseField r 8 numeric
    didtOffset    <- parseField r 9 numeric
    didtFlags     <- parseField r 10 numeric
    didtExtraData <- mdForwardRefOrNull ctx mt <$> parseField r 11 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoDerivedType DIDerivedType{..})) pm

  18 -> label "METADATA_COMPOSITE_TYPE" $ do
    ctx <- getContext
    isDistinct         <- parseField r 0 nonzero
    dictTag            <- parseField r 1 numeric
    dictName           <- mdStringOrNull     ctx mt <$> parseField r 2 numeric
    dictFile           <- mdForwardRefOrNull ctx mt <$> parseField r 3 numeric
    dictLine           <- parseField r 4 numeric
    dictScope          <- mdForwardRefOrNull ctx mt <$> parseField r 5 numeric
    dictBaseType       <- mdForwardRefOrNull ctx mt <$> parseField r 6 numeric
    dictSize           <- parseField r 7 numeric
    dictAlign          <- parseField r 8 numeric
    dictOffset         <- parseField r 9 numeric
    dictFlags          <- parseField r 10 numeric
    dictElements       <- mdForwardRefOrNull ctx mt <$> parseField r 11 numeric
    dictRuntimeLang    <- parseField r 12 numeric
    dictVTableHolder   <- mdForwardRefOrNull ctx mt <$> parseField r 13 numeric
    dictTemplateParams <- mdForwardRefOrNull ctx mt <$> parseField r 14 numeric
    dictIdentifier     <- mdStringOrNull     ctx mt <$> parseField r 15 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoCompositeType DICompositeType{..})) pm

  19 -> label "METADATA_SUBROUTINE_TYPE" $ do
    ctx <- getContext
    isDistinct    <- parseField r 0 nonzero
    distFlags     <- parseField r 1 numeric
    distTypeArray <- mdForwardRefOrNull ctx mt <$> parseField r 2 numeric
    return $! updateMetadataTable
      (addDebugInfo
         isDistinct
         (DebugInfoSubroutineType DISubroutineType{..}))
      pm

  20 -> label "METADATA_COMPILE_UNIT" $ do
    let recordSize = length (recordFields r)
    when (recordSize < 14 || recordSize > 16)
      (fail "Invalid record")

    ctx <- getContext
    isDistinct             <- parseField r 0 nonzero
    dicuLanguage           <- parseField r 1 numeric
    dicuFile               <-
      mdForwardRefOrNull ctx mt <$> parseField r 2 numeric
    dicuProducer           <- mdStringOrNull ctx mt <$> parseField r 3 numeric
    dicuIsOptimized        <- parseField r 4 nonzero
    dicuFlags              <- parseField r 5 numeric
    dicuRuntimeVersion     <- parseField r 6 numeric
    dicuSplitDebugFilename <- mdStringOrNull ctx mt <$> parseField r 7 numeric
    dicuEmissionKind       <- parseField r 8 numeric
    dicuEnums              <-
      mdForwardRefOrNull ctx mt <$> parseField r 9 numeric
    dicuRetainedTypes      <-
      mdForwardRefOrNull ctx mt <$> parseField r 10 numeric
    dicuSubprograms        <-
      mdForwardRefOrNull ctx mt <$> parseField r 11 numeric
    dicuGlobals            <-
      mdForwardRefOrNull ctx mt <$> parseField r 12 numeric
    dicuImports            <-
      mdForwardRefOrNull ctx mt <$> parseField r 13 numeric
    dicuMacros <-
      if recordSize <= 15
      then pure Nothing
      else mdForwardRefOrNull ctx mt <$> parseField r 15 numeric
    dicuDWOId <-
      if recordSize <= 14
      then pure 0
      else parseField r 14 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoCompileUnit DICompileUnit {..})) pm


  21 -> label "METADATA_SUBPROGRAM" $ do
    -- this one is a bit funky:
    -- https://github.com/llvm-mirror/llvm/blob/release_38/lib/Bitcode/Reader/BitcodeReader.cpp#L2186
    let recordSize = length (recordFields r)
        adj i | recordSize == 19 = i + 1
              | otherwise        = i
    when (recordSize /= 18 && recordSize /= 19)
      (fail "Invalid record")

    ctx <- getContext
    isDistinct         <- parseField r 0 nonzero
    dispScope          <- mdForwardRefOrNull ctx mt <$> parseField r 1 numeric
    dispName           <- mdStringOrNull ctx mt <$> parseField r 2 numeric
    dispLinkageName    <- mdStringOrNull ctx mt <$> parseField r 3 numeric
    dispFile           <- mdForwardRefOrNull ctx mt <$> parseField r 4 numeric
    dispLine           <- parseField r 5 numeric
    dispType           <- mdForwardRefOrNull ctx mt <$> parseField r 6 numeric
    dispIsLocal        <- parseField r 7 nonzero
    dispIsDefinition   <- parseField r 8 nonzero
    dispScopeLine      <- parseField r 9 numeric
    dispContainingType <- mdForwardRefOrNull ctx mt <$> parseField r 10 numeric
    dispVirtuality     <- parseField r 11 numeric
    dispVirtualIndex   <- parseField r 12 numeric
    dispFlags          <- parseField r 13 numeric
    dispIsOptimized    <- parseField r 14 nonzero
    dispTemplateParams <-
      mdForwardRefOrNull ctx mt <$> parseField r (adj 15) numeric
    dispDeclaration <-
      mdForwardRefOrNull ctx mt <$> parseField r (adj 16) numeric
    dispVariables <-
      mdForwardRefOrNull ctx mt <$> parseField r (adj 17) numeric
    -- TODO: in the LLVM parser, it then goes into the metadata table
    -- and updates function entries to point to subprograms. Is that
    -- neccessary for us?
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoSubprogram DISubprogram{..})) pm

  22 -> label "METADATA_LEXICAL_BLOCK" $ do
    when (length (recordFields r) /= 5)
      (fail "Invalid record")
    cxt <- getContext
    isDistinct <- parseField r 0 nonzero
    dilbScope  <- mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
    dilbFile   <- mdForwardRefOrNull cxt mt <$> parseField r 2 numeric
    dilbLine   <- parseField r 3 numeric
    dilbColumn <- parseField r 4 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoLexicalBlock DILexicalBlock{..})) pm

  23 -> label "METADATA_LEXICAL_BLOCK_FILE" $ do
    when (length (recordFields r) /= 4)
      (fail "Invalid record")
    cxt <- getContext
    isDistinct <- parseField r 0 nonzero
    dilbfScope <- do
      mScope <- mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
      maybe (fail "Invalid record: scope field not present") return mScope
    dilbfFile <- mdForwardRefOrNull cxt mt <$> parseField r 2 numeric
    dilbfDiscriminator <- parseField r 3 numeric
    return $! updateMetadataTable
      (addDebugInfo
         isDistinct
         (DebugInfoLexicalBlockFile DILexicalBlockFile{..}))
      pm

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
    when (length (recordFields r) /= 11)
      (fail "Invalid record")

    ctx <- getContext
    isDistinct <- parseField r 0 nonzero
    digvScope  <- mdForwardRefOrNull ctx mt <$> parseField r 1 numeric
    digvName   <- mdStringOrNull ctx mt <$> parseField r 2 numeric
    digvLinkageName <- mdStringOrNull ctx mt <$> parseField r 3 numeric
    digvFile   <- mdForwardRefOrNull ctx mt <$> parseField r 4 numeric
    digvLine   <- parseField r 5 numeric
    digvType   <- mdForwardRefOrNull ctx mt <$> parseField r 6 numeric
    digvIsLocal <- parseField r 7 nonzero
    digvIsDefinition <- parseField r 8 nonzero
    digvVariable <- mdForwardRefOrNull ctx mt <$> parseField r 9 numeric
    digvDeclaration <- mdForwardRefOrNull ctx mt <$> parseField r 10 numeric
    return $! updateMetadataTable
      (addDebugInfo
         isDistinct
         (DebugInfoGlobalVariable DIGlobalVariable{..})) pm

  28 -> label "METADATA_LOCAL_VAR" $ do
    -- this one is a bit funky:
    -- https://github.com/llvm-mirror/llvm/blob/release_38/lib/Bitcode/Reader/BitcodeReader.cpp#L2308
    let recordSize = length (recordFields r)
        adj i | recordSize > 8 = i + 1
              | otherwise      = i
    when (recordSize < 8 || recordSize > 10)
      (fail "Invalid record")

    ctx <- getContext
    isDistinct <- parseField r 0 nonzero
    dilvScope  <- mdForwardRefOrNull ctx mt <$> parseField r (adj 1) numeric
    dilvName   <- mdStringOrNull ctx mt <$> parseField r (adj 2) numeric
    dilvFile   <- mdForwardRefOrNull ctx mt <$> parseField r (adj 3) numeric
    dilvLine   <- parseField r (adj 4) numeric
    dilvType   <- mdForwardRefOrNull ctx mt <$> parseField r (adj 5) numeric
    dilvArg    <- parseField r (adj 6) numeric
    dilvFlags  <- parseField r (adj 7) numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoLocalVariable DILocalVariable{..})) pm

  29 -> label "METADATA_EXPRESSION" $ do
    let recordSize = length (recordFields r)
    when (recordSize < 1)
      (fail "Invalid record")
    isDistinct <- parseField r 0 nonzero
    dieElements <- parseFields r 1 numeric
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoExpression DIExpression{..})) pm

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

parseAttachment :: Record -> Int -> Parse [(PKindMd,PValMd)]
parseAttachment r l = loop (length (recordFields r) - 1) []
  where
  loop n acc | n < l = return acc
             | otherwise = do
    kind <- parseField r (n - 1) numeric
    md   <- getMetadata =<< parseField r n numeric
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
