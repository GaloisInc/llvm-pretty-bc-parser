{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Metadata (
    parseMetadataBlock
  , parseMetadataKindEntry
  , PartialUnnamedMd(..)
  , finalizePartialUnnamedMd
  , finalizePValMd
  , InstrMdAttachments
  , PFnMdAttachments
  , PKindMd
  , PGlobalAttachments
  ) where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST
import Text.LLVM.Labels

import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import Control.Exception (throw)
import Control.Monad (foldM,guard,mplus,unless,when)
import Data.Functor.Compose (Compose(..), getCompose)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Bits (shiftR, testBit, shiftL)
import Data.Word (Word32,Word64)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.Map as Map


-- Parsing State ---------------------------------------------------------------

data MetadataTable = MetadataTable
  { mtEntries   :: MdTable
  , mtNextNode  :: !Int
  , mtNodes     :: Map.Map Int (Bool,Bool,Int)
                   -- ^ The entries in the map are: is the entry function local,
                   -- is the entry distinct, and the implicit id for the node.
  } deriving (Show)

emptyMetadataTable ::
  Int {- ^ globals seen so far -} ->
  MdTable -> MetadataTable
emptyMetadataTable globals es = MetadataTable
  { mtEntries   = es
  , mtNextNode  = globals
  , mtNodes     = Map.empty
  }

metadata :: PValMd -> Typed PValue
metadata  = Typed (PrimType Metadata) . ValMd

addMetadata :: PValMd  -> MetadataTable -> (Int,MetadataTable)
addMetadata val mt = (ix, mt { mtEntries = es' })
  where
  (ix,es') = addValue' (metadata val) (mtEntries mt)

addMdValue :: Typed PValue -> MetadataTable -> MetadataTable
addMdValue tv mt = mt { mtEntries = addValue tv' (mtEntries mt) }
  where
  -- explicitly make a metadata value out of a normal value
  tv' = Typed { typedType  = PrimType Metadata
              , typedValue = ValMd (ValMdValue tv)
              }

nameNode :: Bool -> Bool -> Int -> MetadataTable -> MetadataTable
nameNode fnLocal isDistinct ix mt = mt
  { mtNodes    = Map.insert ix (fnLocal,isDistinct,mtNextNode mt) (mtNodes mt)
  , mtNextNode = mtNextNode mt + 1
  }

addString :: String -> MetadataTable -> MetadataTable
addString str = snd . addMetadata (ValMdString str)

addStrings :: [String] -> MetadataTable -> MetadataTable
addStrings strs mt = foldl (flip addString) mt strs

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
  , pmGlobalAttachments:: PGlobalAttachments
  } deriving (Show)

emptyPartialMetadata ::
  Int {- ^ globals seen so far -} ->
  MdTable -> PartialMetadata
emptyPartialMetadata globals es = PartialMetadata
  { pmEntries          = emptyMetadataTable globals es
  , pmNamedEntries     = Map.empty
  , pmNextName         = Nothing
  , pmInstrAttachments = Map.empty
  , pmFnAttachments    = Map.empty
  , pmGlobalAttachments= Map.empty
  }

updateMetadataTable :: (MetadataTable -> MetadataTable)
                    -> (PartialMetadata -> PartialMetadata)
updateMetadataTable f pm = pm { pmEntries = f (pmEntries pm) }

addGlobalAttachments ::
  Symbol {- ^ name of the global to attach to ^ -} ->
  (Map.Map KindMd PValMd) {- ^ metadata references to attach ^ -} ->
  (PartialMetadata -> PartialMetadata)
addGlobalAttachments sym mds pm =
  pm { pmGlobalAttachments = Map.insert sym mds (pmGlobalAttachments pm)
     }

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
  { pumIndex    :: Int
  , pumValues   :: PValMd
  , pumDistinct :: Bool
  } deriving (Show)

finalizePartialUnnamedMd :: PartialUnnamedMd -> Parse UnnamedMd
finalizePartialUnnamedMd pum = mkUnnamedMd `fmap` finalizePValMd (pumValues pum)
  where
  mkUnnamedMd v = UnnamedMd
    { umIndex  = pumIndex pum
    , umValues = v
    , umDistinct = pumDistinct pum
    }

finalizePValMd :: PValMd -> Parse ValMd
finalizePValMd = relabel (const requireBbEntryName)

-- | Partition unnamed entries into global and function local unnamed entries.
unnamedEntries :: PartialMetadata -> ([PartialUnnamedMd],[PartialUnnamedMd])
unnamedEntries pm = foldl resolveNode ([],[]) (Map.toList (mtNodes mt))
  where
  mt = pmEntries pm

  resolveNode (gs,fs) (ref,(fnLocal,d,ix)) = case lookupNode ref d ix of
    Just pum | fnLocal   -> (gs,pum:fs)
             | otherwise -> (pum:gs,fs)

    -- TODO: is this silently eating errors with metadata that's not in the
    -- value table?
    Nothing              -> (gs,fs)

  lookupNode ref d ix = do
    Typed { typedValue = ValMd v } <- lookupValueTableAbs ref (mtEntries mt)
    return PartialUnnamedMd
      { pumIndex  = ix
      , pumValues = v
      , pumDistinct = d
      }

type InstrMdAttachments = Map.Map Int [(KindMd,PValMd)]

type PKindMd = Int
type PFnMdAttachments = Map.Map PKindMd PValMd
type PGlobalAttachments = Map.Map Symbol (Map.Map KindMd PValMd)

type ParsedMetadata =
  ( [NamedMd]
  , ([PartialUnnamedMd],[PartialUnnamedMd])
  , InstrMdAttachments
  , PFnMdAttachments
  , PGlobalAttachments
  )

parsedMetadata :: PartialMetadata -> ParsedMetadata
parsedMetadata pm =
  ( namedEntries pm
  , unnamedEntries pm
  , pmInstrAttachments pm
  , pmFnAttachments pm
  , pmGlobalAttachments pm
  )

-- Applicative composition ------------------------------------------------------------

-- Some utilities for dealing with composition of applicatives

-- | These are useful for avoiding writing 'Compose'
(<$$>) :: forall f g a b. (Functor f, Functor g)
       => (a -> b) -> (f (g a)) -> Compose f g b
h <$$> x = h <$> Compose x

(<**>) :: forall f g a b. (Applicative f, Applicative g)
       => Compose f g (a -> b) -> (f (g a)) -> Compose f g b
h <**> x = h <*> Compose x

-- | These are useful for avoiding writing 'pure'
-- (i.e. only some parts of your long applicative chain use both effects)
(<<*>) :: forall f g a b. (Applicative f, Applicative g)
       => Compose f g (a -> b) -> (f a) -> Compose f g b
h <<*> x = h <*> Compose (pure <$> x)

(<*>>) :: forall f g a b. (Applicative f, Applicative g)
       => Compose f g (a -> b) -> (g a) -> Compose f g b
h <*>> x = h <*> Compose (pure x)

-- Metadata Parsing ------------------------------------------------------------

parseMetadataBlock ::
  Int {- ^ globals seen so far -} ->
  ValueTable -> [Entry] -> Parse ParsedMetadata
parseMetadataBlock globals vt es = label "METADATA_BLOCK" $ do
  ms <- getMdTable
  let pm0 = emptyPartialMetadata globals ms
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
parseMetadataEntry vt mt pm (fromEntry -> Just r) =
 case recordCode r of
  -- [values]
  1 -> label "METADATA_STRING" $ do
    str <- fmap UTF8.decode (parseFields r 0 char) `mplus` parseField r 0 string
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
    name <- fmap UTF8.decode (parseFields r 0 char) `mplus` parseField r 0 cstring
    return $! setNextName name pm

  -- [n x md num]
  5 -> label "METADATA_DISTINCT_NODE" (parseMetadataNode True mt r pm)

  -- [n x [id, name]]
  6 -> label "METADATA_KIND" $ do
    kind <- parseField r 0 numeric
    name <- UTF8.decode <$> parseFields r 1 char
    addKind kind name
    return pm

  -- [distinct, line, col, scope, inlined-at?]
  7 -> label "METADATA_LOCATION" $ do
    -- TODO: broken in 3.7+; needs to be a DILocation rather than an
    -- MDLocation, but there appears to be no difference in the
    -- bitcode. /sigh/

    when (length (recordFields r) /= 5)
      (fail "Invalid record")

    let field = parseField r
    cxt        <- getContext
    isDistinct <- field 0 nonzero
    loc        <- DebugLoc
      <$> field 1 numeric                                 -- dlLine
      <*> field 2 numeric                                 -- dlCol
      <*> (mdForwardRef       cxt mt <$> field 3 numeric) -- dlScope
      <*> (mdForwardRefOrNull cxt mt <$> field 4 numeric) -- dlIA
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
    if recordSize `mod` 2 == 0
    then label "function attachment" $ do
      att <- Map.fromList <$> parseAttachment r 0
      return $! addFnAttachment att pm
    else label "instruction attachment" $ do
      inst <- parseField r 0 numeric
      patt <- parseAttachment r 1
      att <- mapM (\(k,md) -> (,md) <$> getKind k) patt
      return $! addInstrAttachment inst att pm

  12 -> label "METADATA_GENERIC_DEBUG" $ do
    --isDistinct <- parseField r 0 numeric
    --tag <- parseField r 1 numeric
    --version <- parseField r 2 numeric
    --header <- parseField r 3 string
    -- TODO: parse all remaining fields
    fail "not yet implemented"

  13 -> label "METADATA_SUBRANGE" $ do

    when (length (recordFields r) /= 3)
      (fail "Invalid record")

    isDistinct     <- parseField r 0 nonzero
    diNode         <- DISubrange
      <$> parseField r 1 numeric     -- disrCount
      <*> parseField r 2 signedInt64 -- disrLowerBound
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoSubrange diNode)) pm

  -- [distinct, value, name]
  14 -> label "METADATA_ENUMERATOR" $ do

    when (length (recordFields r) /= 3)
      (fail "Invalid record")

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero
    diEnum     <- flip DebugInfoEnumerator
      <$> parseField r 1 signedInt64                   -- value
      <*> (mdString ctx mt <$> parseField r 2 numeric) -- name
    return $! updateMetadataTable (addDebugInfo isDistinct diEnum) pm

  15 -> label "METADATA_BASIC_TYPE" $ do

    when (length (recordFields r) /= 6)
      (fail "Invalid record")

    ctx <- getContext
    isDistinct <- parseField r 0 nonzero
    dibt <- DIBasicType
      <$> parseField r 1 numeric                       -- dibtTag
      <*> (mdString ctx mt <$> parseField r 2 numeric) -- dibtName
      <*> parseField r 3 numeric                       -- dibtSize
      <*> parseField r 4 numeric                       -- dibtAlign
      <*> parseField r 5 numeric                       -- dibtEncoding
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoBasicType dibt)) pm

  -- [distinct, filename, directory]
  16 -> label "METADATA_FILE" $ do

    let recordSize = length (recordFields r)
    when (recordSize /= 3 && recordSize /= 5)
      (fail "Invalid record")

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero
    diFile     <- DIFile
      <$> (mdString ctx mt <$> parseField r 1 numeric) -- difFilename
      <*> (mdString ctx mt <$> parseField r 2 numeric) -- difDirectory
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoFile diFile)) pm

  17 -> label "METADATA_DERIVED_TYPE" $ do

    let recordSize = length (recordFields r)
    when (recordSize < 12 || recordSize > 13)
      (fail "Invalid record")

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero
    didt       <- DIDerivedType
      <$> parseField r 1 numeric                                  -- didtTag
      <*> (mdStringOrNull     ctx mt <$> parseField r 2 numeric)  -- didtName
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 3 numeric)  -- didtFile
      <*> parseField r 4 numeric                                  -- didtLine
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 5 numeric)  -- didtScope
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)  -- didtBaseType
      <*> parseField r 7 numeric                                  -- didtSize
      <*> parseField r 8 numeric                                  -- didtAlign
      <*> parseField r 9 numeric                                  -- didtOffset
      <*> parseField r 10 numeric                                 -- didtFlags
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 11 numeric) -- didtExtraData
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoDerivedType didt)) pm

  18 -> label "METADATA_COMPOSITE_TYPE" $ do

    when (length (recordFields r) /= 16)
      (fail "Invalid record")

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero
    dict       <- DICompositeType
      <$> parseField r 1 numeric                                  -- dictTag
      <*> (mdStringOrNull     ctx mt <$> parseField r 2 numeric)  -- dictName
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 3 numeric)  -- dictFile
      <*> parseField r 4 numeric                                  -- dictLine
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 5 numeric)  -- dictScope
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)  -- dictBaseType
      <*> parseField r 7 numeric                                  -- dictSize
      <*> parseField r 8 numeric                                  -- dictAlign
      <*> parseField r 9 numeric                                  -- dictOffset
      <*> parseField r 10 numeric                                 -- dictFlags
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 11 numeric) -- dictElements
      <*> parseField r 12 numeric                                 -- dictRuntimeLang
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 13 numeric) -- dictVTableHolder
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 14 numeric) -- dictTemplateParams
      <*> (mdStringOrNull     ctx mt <$> parseField r 15 numeric) -- dictIdentifier
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoCompositeType dict)) pm

  19 -> label "METADATA_SUBROUTINE_TYPE" $ do

    let recordSize = length (recordFields r)
    when (recordSize < 3 || recordSize > 4)
      (fail "Invalid record")

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero
    dist <- DISubroutineType
      <$> parseField r 1 numeric                                 -- distFlags
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 2 numeric) -- distTypeArray
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoSubroutineType dist)) pm

  20 -> label "METADATA_COMPILE_UNIT" $ do
    let recordSize = length (recordFields r)

    when (recordSize < 14 || recordSize > 19)
      (fail "Invalid record")

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero
    dicu       <- DICompileUnit
      <$> parseField r 1 numeric                                  -- dicuLanguage
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 2 numeric)  -- dicuFile
      <*> (mdStringOrNull ctx mt     <$> parseField r 3 numeric)  -- dicuProducer
      <*> parseField r 4 nonzero                                  -- dicuIsOptimized
      <*> (mdStringOrNull ctx mt     <$> parseField r 5 numeric)  -- dicuFlags
      <*> parseField r 6 numeric                                  -- dicuRuntimeVersion
      <*> (mdStringOrNull ctx mt     <$> parseField r 7 numeric)  -- dicuSplitDebugFilename
      <*> parseField r 8 numeric                                  -- dicuEmissionKind
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 9 numeric)  -- dicuEnums
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 10 numeric) -- dicuRetainedTypes
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 11 numeric) -- dicuSubprograms
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 12 numeric) -- dicuGlobals
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 13 numeric) -- dicuImports
      <*> if recordSize <= 15
          then pure Nothing
          else mdForwardRefOrNull ctx mt <$> parseField r 15 numeric -- dicuMacros
    dicuDWOId <-
      if recordSize <= 14
      then pure 0
      else parseField r 14 numeric
    dicuSplitDebugInlining <-
      if recordSize <= 16
      then pure True
      else parseField r 16 nonzero
    let dicu' = dicu dicuDWOId dicuSplitDebugInlining
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoCompileUnit dicu')) pm


  21 -> label "METADATA_SUBPROGRAM" $ do
    -- this one is a bit funky:
    -- https://github.com/llvm-mirror/llvm/blob/release_50/lib/Bitcode/Reader/MetadataLoader.cpp#L1382
    let recordSize = length (recordFields r)
        adj i | recordSize == 19 = i + 1
              | otherwise        = i
        hasThisAdjustment = recordSize >= 20
        hasThrownTypes    = recordSize >= 21
    unless (18 <= recordSize && recordSize <= 21)
      (fail ("Invalid subprogram record, size = " ++ show recordSize))

    ctx        <- getContext
    isDistinct <- parseField r 0 nonzero -- isDistinct
    disp       <- DISubprogram
      <$> (mdForwardRefOrNull ctx mt <$> parseField r 1 numeric)        -- dispScope
      <*> (mdStringOrNull ctx mt <$> parseField r 2 numeric)            -- dispName
      <*> (mdStringOrNull ctx mt <$> parseField r 3 numeric)            -- dispLinkageName
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 4 numeric)        -- dispFile
      <*> parseField r 5 numeric                                        -- dispLine
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)        -- dispType
      <*> parseField r 7 nonzero                                        -- dispIsLocal
      <*> parseField r 8 nonzero                                        -- dispIsDefinition
      <*> parseField r 9 numeric                                        -- dispScopeLine
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 10 numeric)       -- dispContainingType
      <*> parseField r 11 numeric                                       -- dispVirtuality
      <*> parseField r 12 numeric                                       -- dispVirtualIndex
      <*> (if hasThisAdjustment
          then parseField r 19 numeric
          else return 0)                                                -- dispThisAdjustment
      <*> (if hasThrownTypes
          then mdForwardRefOrNull ctx mt <$> parseField r 20 numeric
          else return Nothing)                                          -- dispThrownTypes
      <*> parseField r 13 numeric                                       -- dispFlags
      <*> parseField r 14 nonzero                                       -- dispIsOptimized
      <*> (mdForwardRefOrNull ctx mt <$> parseField r (adj 15) numeric) -- dispTemplateParams
      <*> (mdForwardRefOrNull ctx mt <$> parseField r (adj 16) numeric) -- dispDeclaration
      <*> (mdForwardRefOrNull ctx mt <$> parseField r (adj 17) numeric) -- dispVariables
    -- TODO: in the LLVM parser, it then goes into the metadata table
    -- and updates function entries to point to subprograms. Is that
    -- neccessary for us?
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoSubprogram disp)) pm

  22 -> label "METADATA_LEXICAL_BLOCK" $ do

    when (length (recordFields r) /= 5)
      (fail "Invalid record")

    cxt        <- getContext
    isDistinct <- parseField r 0 nonzero
    dilb       <- DILexicalBlock
      <$> (mdForwardRefOrNull cxt mt <$> parseField r 1 numeric) -- dilbScope
      <*> (mdForwardRefOrNull cxt mt <$> parseField r 2 numeric) -- dilbFile
      <*> parseField r 3 numeric                                 -- dilbLine
      <*> parseField r 4 numeric                                 -- dilbColumn
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoLexicalBlock dilb)) pm

  23 -> label "METADATA_LEXICAL_BLOCK_FILE" $ do

    when (length (recordFields r) /= 4)
      (fail "Invalid record")

    cxt        <- getContext
    isDistinct <- parseField r 0 nonzero
    dilbf      <- getCompose $ DILexicalBlockFile -- Composing (Parse . Maybe)
      <$$> (mdForwardRefOrNull cxt mt <$> parseField r 1 numeric)
      <<*> (mdForwardRefOrNull cxt mt <$> parseField r 2 numeric) -- dilbfFile
      <<*> (parseField r 3 numeric)                               -- dilbfDiscriminator

    case dilbf of
      Just dilbf' ->
        return $! updateMetadataTable
          (addDebugInfo isDistinct (DebugInfoLexicalBlockFile dilbf')) pm
      Nothing -> fail "Invalid record: scope field not present"

  24 -> label "METADATA_NAMESPACE" $ do
    isNew <- case length (recordFields r) of
               3 -> return True
               5 -> return False
               _ -> fail "Invalid METADATA_NAMESPACE record"
    cxt        <- getContext
    isDistinct <- parseField r 0 nonzero
    let nameIdx = if isNew then 2 else 3
    dins       <- DINameSpace
      <$> (mdString cxt mt         <$> parseField r nameIdx numeric) -- dinsName
      <*> (mdForwardRef cxt mt     <$> parseField r 1 numeric)       -- dinsScope
      <*> (if isNew
          then return (ValMdString "")
          else mdForwardRef cxt mt <$> parseField r 2 numeric)       -- dinsFile
      <*> if isNew then return 0 else parseField r 4 numeric         -- dinsLine
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoNameSpace dins)) pm

  25 -> label "METADATA_TEMPLATE_TYPE" $ do

    when (length (recordFields r) /= 3)
      (fail "Invalid record")

    cxt <- getContext
    isDistinct <- parseField r 0 nonzero
    dittp <- DITemplateTypeParameter
      <$> (mdString cxt mt     <$> parseField r 1 numeric) -- dittpName
      <*> (mdForwardRef cxt mt <$> parseField r 2 numeric) -- dittpType
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoTemplateTypeParameter dittp)) pm

  26 -> label "METADATA_TEMPLATE_VALUE" $ do
    cxt <- getContext
    isDistinct <- parseField r 0 nonzero
    ditvp <- DITemplateValueParameter
      <$> (mdString cxt mt     <$> parseField r 1 numeric) -- ditvpName
      <*> (mdForwardRef cxt mt <$> parseField r 2 numeric) -- ditvpType
      <*> (mdForwardRef cxt mt <$> parseField r 3 numeric) -- ditvpValue
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoTemplateValueParameter ditvp)) pm

  27 -> label "METADATA_GLOBAL_VAR" $ do
    let len = length (recordFields r)
    unless (11 <= len && len <= 12)
      (fail "Unexpected number of record fields")

    ctx        <- getContext
    field0     <- parseField r 0 numeric
    let isDistinct = testBit field0 0
        _version   = shiftR  field0 1 :: Int

    digv <- DIGlobalVariable
      <$> (mdForwardRefOrNull ctx mt <$> parseField r 1 numeric)  -- digvScope
      <*> (mdStringOrNull ctx mt     <$> parseField r 2 numeric)  -- digvName
      <*> (mdStringOrNull ctx mt     <$> parseField r 3 numeric)  -- digvLinkageName
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 4 numeric)  -- digvFile
      <*> parseField r 5 numeric                                  -- digvLine
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)  -- digvType
      <*> parseField r 7 nonzero                                  -- digvIsLocal
      <*> parseField r 8 nonzero                                  -- digvIsDefinition
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 9 numeric)  -- digvVariable
      <*> (mdForwardRefOrNull ctx mt <$> parseField r 10 numeric) -- digvDeclaration
      <*> if len > 11
          then Just                  <$> parseField r 11 numeric  -- digvAlignment
          else pure Nothing
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoGlobalVariable digv)) pm

  28 -> label "METADATA_LOCAL_VAR" $ do
    -- this one is a bit funky:
    -- https://github.com/llvm-mirror/llvm/blob/release_38/lib/Bitcode/Reader/BitcodeReader.cpp#L2308
    let recordSize = length (recordFields r)
    when (recordSize < 8 || recordSize > 10)
      (fail "Invalid record")

    ctx <- getContext

    field0 <- parseField r 0 numeric
    let isDistinct   = testBit (field0 :: Word32) 0
        hasAlignment = testBit (field0 :: Word32) 1

        hasTag | not hasAlignment && recordSize > 8 = 1
               | otherwise                          = 0

        adj i = i + hasTag

    _alignInBits <-
      if hasAlignment
         then do n <- parseField r (adj 8) numeric
                 when ((n :: Word64) > fromIntegral (maxBound :: Word32))
                      (fail "Alignment value is too large")
                 return (fromIntegral n :: Word32)

         else return 0

    dilv <- DILocalVariable
      <$> (mdForwardRefOrNull ("dilvScope":ctx) mt
            <$> parseField r (adj 1) numeric) -- dilvScope
      <*> (mdStringOrNull     ("dilvName" :ctx) mt
            <$> parseField r (adj 2) numeric) -- dilvName
      <*> (mdForwardRefOrNull ("dilvFile" :ctx) mt
            <$> parseField r (adj 3) numeric) -- dilvFile
      <*> parseField r (adj 4) numeric        -- dilvLine
      <*> (mdForwardRefOrNull ("dilvType" :ctx) mt
            <$> parseField r (adj 5) numeric) -- dilvType
      <*> parseField r (adj 6) numeric        -- dilvArg
      <*> parseField r (adj 7) numeric        -- dilvFlags
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoLocalVariable dilv)) pm

  29 -> label "METADATA_EXPRESSION" $ do
    let recordSize = length (recordFields r)
    when (recordSize < 1)
      (fail "Invalid record")
    isDistinct <- parseField r 0 nonzero
    diExpr     <- DebugInfoExpression . DIExpression <$> parseFields r 1 numeric
    return $! updateMetadataTable (addDebugInfo isDistinct diExpr) pm

  30 -> label "METADATA_OBJC_PROPERTY" $ do
    -- TODO
    fail "not yet implemented"
  31 -> label "METADATA_IMPORTED_ENTITY" $ do
    cxt        <- getContext
    isDistinct <- parseField r 0 nonzero
    diie       <- DIImportedEntity
      <$> parseField r 1 numeric                                 -- diieTag
      <*> (mdString cxt mt           <$> parseField r 5 numeric) -- diieName
      <*> (mdForwardRefOrNull cxt mt <$> parseField r 2 numeric) -- diieScope
      <*> (mdForwardRefOrNull cxt mt <$> parseField r 3 numeric) -- diieEntity
      <*> parseField r 4 numeric                                 -- diieLine

    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoImportedEntity diie)) pm

  32 -> label "METADATA_MODULE" $ do
    -- cxt <- getContext
    -- isDistinct <- parseField r 0 numeric
    -- mdForwardRefOrNull cxt mt <$> parseField r 1 numeric
    -- parseField r 2 string
    -- parseField r 3 string
    -- parseField r 4 string
    -- parseField r 5 string
    -- TODO
    fail "not yet implemented"
  33 -> label "METADATA_MACRO" $ do
    -- isDistinct <- parseField r 0 numeric
    -- parseField r 1 numeric
    -- parseField r 2 numeric
    -- parseField r 3 string
    -- parseField r 4 string
    -- TODO
    fail "not yet implemented"
  34 -> label "METADATA_MACRO_FILE" $ do
    -- cxt <- getContext
    -- isDistinct <- parseField r 0 numeric
    -- parseField r 1 numeric
    -- parseField r 2 numeric
    -- mdForwardRefOrNull cxt mt <$> parseField r 3 numeric
    -- mdForwardRefOrNull cxt mt <$> parseField r 4 numeric
    -- TODO
    fail "not yet implemented"

  35 -> label "METADATA_STRINGS" $ do
    when (length (recordFields r) /= 3)
      (fail "Invalid record: metadata strings layout")
    count  <- parseField r 0 numeric
    offset <- parseField r 1 numeric
    bs     <- parseField r 2 fieldBlob
    when (count == 0)
      (fail "Invalid record: metadata strings with no strings")
    when (offset >= S.length bs)
      (fail "Invalid record: metadata strings corrupt offset")
    let (bsLengths, bsStrings) = S.splitAt offset bs
    lengths <- either fail return $ parseMetadataStringLengths count bsLengths
    when (sum lengths > S.length bsStrings)
      (fail "Invalid record: metadata strings truncated")
    let strings = snd (mapAccumL f bsStrings lengths)
          where f s i = case S.splitAt i s of
                          (str, rest) -> (rest, Char8.unpack str)
    return $! updateMetadataTable (addStrings strings) pm

  -- [ valueid, n x [id, mdnode] ]
  36 -> label "METADATA_GLOBAL_DECL_ATTACHMENT" $ do

    -- the record will always be of odd length
    when (mod (length (recordFields r)) 2 == 0)
         (fail "Invalid record")

    valueId <- parseField r 0 numeric
    sym     <- case lookupValueTableAbs valueId vt of
                 Just (Typed { typedValue = ValSymbol sym }) -> return sym
                 _ -> fail "Non-global referenced"

    refs <- parseGlobalObjectAttachment mt r

    return $! addGlobalAttachments sym refs pm

  37 -> label "METADATA_GLOBAL_VAR_EXPR" $ do
    when (length (recordFields r) /= 3)
      (fail "Invalid record: unsupported layout")
    cxt <- getContext
    isDistinct      <- parseField r 0 nonzero
    digve <- DIGlobalVariableExpression
      <$> (mdForwardRefOrNull cxt mt <$> parseField r 1 numeric) -- digveVariable
      <*> (mdForwardRefOrNull cxt mt <$> parseField r 2 numeric) -- digveExpression
    return $! updateMetadataTable
      (addDebugInfo isDistinct (DebugInfoGlobalVariableExpression digve)) pm

  38 -> label "METADATA_INDEX_OFFSET" $ do

    when (length (recordFields r) /= 2)
         (fail "Invalid record")

    a <- parseField r 0 numeric
    b <- parseField r 1 numeric
    let _offset = a + (b `shiftL` 32) :: Word64

    -- TODO: is it OK to skip this if we always parse everything?
    return pm


  -- In the llvm source, this node is processed when the INDEX_OFFSET record is
  -- found.
  39 -> label "METADATA_INDEX" $ do
    -- TODO: is it OK to skip this if we always parse everything?
    return pm

  code -> fail ("unknown record code: " ++ show code)

parseMetadataEntry _ _ pm (abbrevDef -> Just _) =
  return pm

parseMetadataEntry _ _ _ r =
  fail ("unexpected metadata entry: " ++ show r)

parseAttachment :: Record -> Int -> Parse [(PKindMd,PValMd)]
parseAttachment r l = loop (length (recordFields r) - 1) []
  where
  loop n acc | n < l = return acc
             | otherwise = do
    kind <- parseField r (n - 1) numeric
    md   <- getMetadata =<< parseField r n numeric
    loop (n - 2) ((kind,typedValue md) : acc)


-- | This is a named version of the metadata list that can show up at the end of
-- a global declaration. It will be of the form @!dbg !2 [!dbg !n, ...]@.
parseGlobalObjectAttachment :: MetadataTable -> Record -> Parse (Map.Map KindMd PValMd)
parseGlobalObjectAttachment mt r = label "parseGlobalObjectAttachment" $
  do cxt <- getContext
     go cxt Map.empty 1
  where
  len = length (recordFields r)

  go cxt acc n | n < len =
    do kind <- getKind =<< parseField r n numeric
       i    <- parseField r (n + 1) numeric
       go cxt (Map.insert kind (mdForwardRef cxt mt i) acc) (n + 2)

  go _ acc _ =
       return acc


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
  addKind kind (UTF8.decode name)
