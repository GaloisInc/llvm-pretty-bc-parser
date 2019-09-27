{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
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
  , dedupMetadata
  , InstrMdAttachments
  , PFnMdAttachments
  , PKindMd
  , PGlobalAttachments
  ) where

import           Data.LLVM.BitCode.Bitstream
import           Data.LLVM.BitCode.Match
import           Data.LLVM.BitCode.Parse
import           Data.LLVM.BitCode.Record
import           Text.LLVM.AST
import           Text.LLVM.Labels

import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import           Control.Applicative ((<|>))
import           Control.Exception (throw)
import           Control.Monad (foldM, guard, mplus, when)
import           Data.Bits (shiftR, testBit, shiftL)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as Char8 (unpack)
import           Data.Either (partitionEithers)
import           Data.Functor.Compose (Compose(..), getCompose)
import           Data.Generics.Uniplate.Data
import qualified Data.IntMap as IntMap
import           Data.List (mapAccumL, foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Word (Word32,Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack)



-- Parsing State ---------------------------------------------------------------

data MetadataTable = MetadataTable
  { mtEntries   :: MdTable
  , mtNextNode  :: !Int
  , mtNodes     :: IntMap.IntMap (Bool, Bool, Int)
                   -- ^ The entries in the map are: is the entry function local,
                   -- is the entry distinct, and the implicit id for the node.
  } deriving (Show)

emptyMetadataTable ::
  Int {- ^ globals seen so far -} ->
  MdTable -> MetadataTable
emptyMetadataTable globals es = MetadataTable
  { mtEntries   = es
  , mtNextNode  = globals
  , mtNodes     = IntMap.empty
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
  { mtNodes    = IntMap.insert ix (fnLocal,isDistinct,mtNextNode mt) (mtNodes mt)
  , mtNextNode = mtNextNode mt + 1
  }

addString :: String -> PartialMetadata -> PartialMetadata
addString str pm =
  let (ix, mt) = addMetadata (ValMdString str) (pmEntries pm)
  in pm { pmEntries = mt
        , pmStrings = Map.insert ix str (pmStrings pm)
        }

addStrings :: [String] -> PartialMetadata -> PartialMetadata
addStrings strs pm = foldl' (flip addString) pm strs

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
  nodeRef           = reference `fmap` IntMap.lookup ix (mtNodes mt)
  fallback          = case forwardRef cxt ix (mtEntries mt) of
                        Typed { typedValue = ValMd md } -> md
                        tv                              -> ValMdValue tv
  reference (False, _, r) = ValMdRef r
  reference (_    , _, r) =
    let explanation = "Illegal forward reference into function-local metadata."
    in throw (BadValueRef callStack cxt explanation r)

mdForwardRefOrNull :: [String] -> MetadataTable -> Int -> Maybe PValMd
mdForwardRefOrNull cxt mt ix | ix > 0 = Just (mdForwardRef cxt mt (ix - 1))
                             | otherwise = Nothing

mdNodeRef :: HasCallStack
          => [String] -> MetadataTable -> Int -> Int
mdNodeRef cxt mt ix = maybe except prj (IntMap.lookup ix (mtNodes mt))
  where explanation   = "Bad forward reference into mtNodes"
        except        = throw (BadValueRef callStack cxt explanation ix)
        prj (_, _, x) = x

mdString :: HasCallStack
         => [String] -> PartialMetadata -> Int -> String
mdString cxt partialMeta ix =
  let explanation = "Null value when metadata string was expected"
  in fromMaybe (throw (BadValueRef callStack cxt explanation ix))
               (mdStringOrNull cxt partialMeta ix)

-- | This preferentially fetches the string from the strict string table
-- (@pmStrings@), but will return a forward reference when it can't find it there.
mdStringOrNull :: HasCallStack
               => [String]
               -> PartialMetadata
               -> Int
               -> Maybe String
mdStringOrNull cxt partialMeta ix =
  Map.lookup (ix - 1) (pmStrings partialMeta) <|>
    case mdForwardRefOrNull cxt (pmEntries partialMeta) ix of
      Nothing                -> Nothing
      Just (ValMdString str) -> Just str
      Just _                 ->
        let explanation = "Non-string metadata when string was expected"
        in throw (BadTypeRef callStack cxt explanation ix)

mdStringOrEmpty :: HasCallStack
                => [String]
                -> PartialMetadata
                -> Int
                -> String
mdStringOrEmpty cxt partialMeta = fromMaybe "" . mdStringOrNull cxt partialMeta

mkMdRefTable :: MetadataTable -> MdRefTable
mkMdRefTable mt = IntMap.mapMaybe step (mtNodes mt)
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
  , pmStrings          :: Map Int String
  -- ^ Forward references to metadata strings are never actually
  -- forward references, string blocks (@METADATA_STRINGS@) always come first.
  -- So references to them don't need to be inside the @MonadFix@ like
  -- references into other 'pmEntries', allowing them to be strict.
  --
  -- See this comment:
  -- - https://github.com/llvm-mirror/llvm/blob/release_40/lib/Bitcode/Reader/MetadataLoader.cpp#L913
  -- - https://github.com/llvm-mirror/llvm/blob/release_60/lib/Bitcode/Reader/MetadataLoader.cpp#L1017
  } deriving (Show)

emptyPartialMetadata ::
  Int {- ^ globals seen so far -} ->
  MdTable -> PartialMetadata
emptyPartialMetadata globals es = PartialMetadata
  { pmEntries           = emptyMetadataTable globals es
  , pmNamedEntries      = Map.empty
  , pmNextName          = Nothing
  , pmInstrAttachments  = Map.empty
  , pmFnAttachments     = Map.empty
  , pmGlobalAttachments = Map.empty
  , pmStrings           = Map.empty
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

-- De-duplicating ---------------------------------------------------------------

-- | This function generically traverses the given unnamed metadata values.
-- When it encounters one with a 'PValMd' inside of it, it looks up that
-- value in the list. If found, it replaces the value with a reference to it.
--
-- Such de-duplication is necessary because the @fallback@ of
-- 'mdForwardRefOrNull' is often called when it is in fact unnecessary, just
-- because the appropriate references aren't available yet.
--
-- This function is concise at the cost of efficiency: In the worst case, every
-- metadata node contains a reference to every other metadata node, and the
-- cost is O(n^2*log(n)) where
-- * n^2 comes from looking at every 'PValMd' inside every 'PartialUnnamedMd'
-- * log(n) is the cost of looking them up in a 'Map'.
dedupMetadata :: [PartialUnnamedMd] -> [PartialUnnamedMd]
dedupMetadata pumd = map (helper (mkPartialUnnamedMdMap pumd)) pumd
  where helper pumdMap pum =
          let pumdMap' = Map.delete (pumValues pum) pumdMap -- don't self-reference
          in pum { pumValues = maybeTransform pumdMap' (pumValues pum) }

        -- | We avoid erroneously recursing into ValMdValues and exit early on
        -- a few other constructors de-duplication wouldn't affect.
        maybeTransform :: Map PValMd Int -> PValMd -> PValMd
        maybeTransform pumdMap v@(ValMdNode _)      = transform (trans pumdMap) v
        maybeTransform pumdMap v@(ValMdLoc _)       = transform (trans pumdMap) v
        maybeTransform pumdMap v@(ValMdDebugInfo _) = transform (trans  pumdMap) v
        maybeTransform _       v                    = v

        trans :: Map PValMd Int -> PValMd -> PValMd
        trans pumdMap v = case Map.lookup v pumdMap of
                            Just idex -> ValMdRef idex
                            Nothing   -> v

        mkPartialUnnamedMdMap :: [PartialUnnamedMd] -> Map PValMd Int
        mkPartialUnnamedMdMap =
          foldl' (\mp part -> Map.insert (pumValues part) (pumIndex part) mp) Map.empty

-- Finalizing ---------------------------------------------------------------

namedEntries :: PartialMetadata -> [NamedMd]
namedEntries  = map (uncurry NamedMd)
              . Map.toList
              . pmNamedEntries

data PartialUnnamedMd = PartialUnnamedMd
  { pumIndex    :: Int
  , pumValues   :: PValMd
  , pumDistinct :: Bool
  } deriving (Data, Eq, Ord, Generic, Show, Typeable)

finalizePartialUnnamedMd :: PartialUnnamedMd -> Finalize UnnamedMd
finalizePartialUnnamedMd pum = mkUnnamedMd `fmap` finalizePValMd (pumValues pum)
  where
  mkUnnamedMd v = UnnamedMd
    { umIndex  = pumIndex pum
    , umValues = v
    , umDistinct = pumDistinct pum
    }

finalizePValMd :: PValMd -> Finalize ValMd
finalizePValMd = relabel (const requireBbEntryName)

-- | Partition unnamed entries into global and function local unnamed entries.
unnamedEntries :: PartialMetadata -> ([PartialUnnamedMd], [PartialUnnamedMd])
unnamedEntries pm = partitionEithers (mapMaybe resolveNode (IntMap.toList (mtNodes mt)))
  where
  mt = pmEntries pm

  -- TODO: is this silently eating errors with metadata that's not in the
  -- value table (when the lookupValueTableAbs fails)?
  resolveNode (ref,(fnLocal,d,ix)) =
    ((if fnLocal then Right else Left) <$> lookupNode ref d ix)

  lookupNode ref d ix = flip fmap (lookupValueTableAbs ref (mtEntries mt)) $
    \case
      Typed { typedValue = ValMd v } ->
        PartialUnnamedMd
          { pumIndex    = ix
          , pumValues   = v
          , pumDistinct = d
          }
      _ -> error "Impossible: Only ValMds are stored in mtEntries"

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

-- | These are useful for avoiding writing 'pure'
-- (i.e. only some parts of your long applicative chain use both effects)
(<<*>) :: forall f g a b. (Applicative f, Applicative g)
       => Compose f g (a -> b) -> (f a) -> Compose f g b
h <<*> x = h <*> Compose (pure <$> x)

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
--
-- Based on the function 'parseOneMetadata' in the LLVM source.
parseMetadataEntry :: ValueTable -> MetadataTable -> PartialMetadata -> Entry
                   -> Parse PartialMetadata
parseMetadataEntry vt mt pm (fromEntry -> Just r) =
  let msg = [ "Are you sure you're using a supported version of LLVM/Clang?"
            , "Check here: https://github.com/GaloisInc/llvm-pretty-bc-parser"
            ]
      assertRecordSizeBetween lb ub =
        let len = length (recordFields r)
        in when (len < lb || ub < len) $
             fail $ unlines $ [ "Invalid record size: " ++ show len
                              , "Expected size between " ++ show lb ++ " and " ++ show ub
                              ] ++ msg
      assertRecordSizeIn ns =
        let len = length (recordFields r)
        in when (not (len `elem` ns)) $
             fail $ unlines $ [ "Invalid record size: " ++ show len
                              , "Expected one of: " ++ show ns
                              ] ++ msg
  in case recordCode r of
    -- [values]
    1 -> label "METADATA_STRING" $ do
      str <- fmap UTF8.decode (parseFields r 0 char) `mplus` parseField r 0 string
      return $! addString str pm

    -- [type num, value num]
    2 -> label "METADATA_VALUE" $ do
      assertRecordSizeIn [2]
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
      assertRecordSizeIn [5, 6]
      let field = parseField r
      cxt        <- getContext
      isDistinct <- field 0 nonzero
      loc        <- DebugLoc
        <$> field 1 numeric                                 -- dlLine
        <*> field 2 numeric                                 -- dlCol
        <*> (mdForwardRef       cxt mt <$> field 3 numeric) -- dlScope
        <*> (mdForwardRefOrNull cxt mt <$> field 4 numeric) -- dlIA
        <*> if length (recordFields r) <= 5
            then pure False
            else parseField r 5 nonzero                     -- dlImplicit
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
      assertRecordSizeIn [3]
      isDistinct     <- parseField r 0 nonzero
      diNode         <- DISubrange
        <$> parseField r 1 numeric     -- disrCount
        <*> parseField r 2 signedInt64 -- disrLowerBound
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoSubrange diNode)) pm

    -- [distinct, value, name]
    14 -> label "METADATA_ENUMERATOR" $ do
      assertRecordSizeIn [3]
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      diEnum     <- flip DebugInfoEnumerator
        <$> parseField r 1 signedInt64                   -- value
        <*> (mdString ctx pm <$> parseField r 2 numeric) -- name
      return $! updateMetadataTable (addDebugInfo isDistinct diEnum) pm

    15 -> label "METADATA_BASIC_TYPE" $ do
      assertRecordSizeIn [6, 7]
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dibt       <- DIBasicType
        <$> parseField r 1 numeric                       -- dibtTag
        <*> (mdString ctx pm <$> parseField r 2 numeric) -- dibtName
        <*> parseField r 3 numeric                       -- dibtSize
        <*> parseField r 4 numeric                       -- dibtAlign
        <*> parseField r 5 numeric                       -- dibtEncoding
        <*> if length (recordFields r) <= 6
            then pure Nothing
            else Just <$> parseField r 6 numeric         -- dibtFlags
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoBasicType dibt)) pm

    -- [distinct, filename, directory]
    16 -> label "METADATA_FILE" $ do
      assertRecordSizeIn [3, 5]
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      diFile     <- DIFile
        <$> (mdStringOrEmpty ctx pm <$> parseField r 1 numeric) -- difFilename
        <*> (mdStringOrEmpty ctx pm <$> parseField r 2 numeric) -- difDirectory
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoFile diFile)) pm

    17 -> label "METADATA_DERIVED_TYPE" $ do
      assertRecordSizeBetween 12 13
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      didt       <- DIDerivedType
        <$> parseField r 1 numeric                                  -- didtTag
        <*> (mdStringOrNull     ctx pm <$> parseField r 2 numeric)  -- didtName
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
      assertRecordSizeBetween 16 17
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dict       <- DICompositeType
        <$> parseField r 1 numeric                                     -- dictTag
        <*> (mdStringOrNull     ctx pm <$> parseField r 2 numeric)     -- dictName
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 3 numeric)     -- dictFile
        <*> parseField r 4 numeric                                     -- dictLine
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 5 numeric)     -- dictScope
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)     -- dictBaseType
        <*> parseField r 7 numeric                                     -- dictSize
        <*> parseField r 8 numeric                                     -- dictAlign
        <*> parseField r 9 numeric                                     -- dictOffset
        <*> parseField r 10 numeric                                    -- dictFlags
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 11 numeric)    -- dictElements
        <*> parseField r 12 numeric                                    -- dictRuntimeLang
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 13 numeric)    -- dictVTableHolder
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 14 numeric)    -- dictTemplateParams
        <*> (mdStringOrNull     ctx pm <$> parseField r 15 numeric)    -- dictIdentifier
        <*> if length (recordFields r) <= 16
            then pure Nothing
            else mdForwardRefOrNull ctx mt <$> parseField r 16 numeric -- dictDiscriminator
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoCompositeType dict)) pm

    19 -> label "METADATA_SUBROUTINE_TYPE" $ do
      assertRecordSizeBetween 3 4
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dist <- DISubroutineType
        <$> parseField r 1 numeric                                 -- distFlags
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 2 numeric) -- distTypeArray
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoSubroutineType dist)) pm

    20 -> label "METADATA_COMPILE_UNIT" $ do
      assertRecordSizeBetween 14 19
      let recordSize = length (recordFields r)
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dicu       <- DICompileUnit
        <$> parseField r 1 numeric                                  -- dicuLanguage
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 2 numeric)  -- dicuFile
        <*> (mdStringOrNull     ctx pm <$> parseField r 3 numeric)  -- dicuProducer
        <*> parseField r 4 nonzero                                  -- dicuIsOptimized
        <*> (mdStringOrNull     ctx pm <$> parseField r 5 numeric)  -- dicuFlags
        <*> parseField r 6 numeric                                  -- dicuRuntimeVersion
        <*> (mdStringOrNull     ctx pm <$> parseField r 7 numeric)  -- dicuSplitDebugFilename
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

      -- A "version" is encoded in the high-order bits of the isDistinct field.
      -- We parse it once here as a numeric value, and later as a Bool.
      version <- parseField r 0 numeric
      assertRecordSizeBetween 18 21
      let recordSize = length (recordFields r)
          adj i | recordSize == 19 = i + 1
                | otherwise        = i
          hasThisAdjustment = recordSize >= 20
          hasThrownTypes    = recordSize >= 21
          hasUnit           = version >= (2 :: Int) -- avoid default type

      ctx          <- getContext

      -- See https://github.com/elliottt/llvm-pretty/issues/47
      -- and the corresponding LLVM code in MetadataLoader.cpp (parseOneMetadata)
      isDefinition <- parseField r 8 nonzero                       -- dispIsDefinition
      isDistinct   <- (isDefinition ||) <$> parseField r 0 nonzero -- isDistinct

      -- Forward references that depend on the 'version'
      let optFwdRef b n =
            if b
            then mdForwardRefOrNull ctx mt <$> parseField r n numeric
            else pure Nothing

      disp         <- DISubprogram
        <$> (mdForwardRefOrNull ctx mt <$> parseField r 1 numeric)        -- dispScope
        <*> (mdStringOrNull     ctx pm <$> parseField r 2 numeric)        -- dispName
        <*> (mdStringOrNull     ctx pm <$> parseField r 3 numeric)        -- dispLinkageName
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 4 numeric)        -- dispFile
        <*>                                parseField r 5 numeric         -- dispLine
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)        -- dispType
        <*>                                parseField r 7 nonzero         -- dispIsLocal
        <*>                                pure isDefinition              -- dispIsDefinition
        <*>                                parseField r 9 numeric         -- dispScopeLine
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 10 numeric)       -- dispContainingType
        <*>                                parseField r 11 numeric        -- dispVirtuality
        <*>                                parseField r 12 numeric        -- dispVirtualIndex
        <*> (if hasThisAdjustment
            then parseField r 19 numeric
            else return 0)                                                -- dispThisAdjustment
        <*> parseField r 13 numeric                                       -- dispFlags
        <*> parseField r 14 nonzero                                       -- dispIsOptimized
        <*> (optFwdRef hasUnit       15)                                  -- dispUnit
        <*> (optFwdRef (not hasUnit) (adj 15))                            -- dispTemplateParams
        <*> (mdForwardRefOrNull ctx mt <$> parseField r (adj 16) numeric) -- dispDeclaration
        <*> (mdForwardRefOrNull ctx mt <$> parseField r (adj 17) numeric) -- dispVariables
        -- Indices 18-19 seem unused.
        <*> (optFwdRef hasThrownTypes 20)                                 -- dispThrownTypes
      -- TODO: in the LLVM parser, it then goes into the metadata table
      -- and updates function entries to point to subprograms. Is that
      -- neccessary for us?
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoSubprogram disp)) pm

    22 -> label "METADATA_LEXICAL_BLOCK" $ do
      assertRecordSizeIn [5]
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
      assertRecordSizeIn [4]
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
      assertRecordSizeIn [3, 5]
      let isNew =
            case length (recordFields r) of
              3 -> True
              5 -> False
              _ -> error "Impossible (METADATA_NAMESPACE)" -- see assertion
      let nameIdx = if isNew then 2 else 3

      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      dins       <- DINameSpace
        <$> (mdStringOrNull cxt pm   <$> parseField r nameIdx numeric) -- dinsName
        <*> (mdForwardRef cxt mt     <$> parseField r 1 numeric)       -- dinsScope
        <*> (if isNew
            then return (ValMdString "")
            else mdForwardRef cxt mt <$> parseField r 2 numeric)       -- dinsFile
        <*> if isNew then return 0 else parseField r 4 numeric         -- dinsLine
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoNameSpace dins)) pm

    25 -> label "METADATA_TEMPLATE_TYPE" $ do
      assertRecordSizeIn [3]
      cxt <- getContext
      isDistinct <- parseField r 0 nonzero
      dittp <- DITemplateTypeParameter
        <$> (mdStringOrNull cxt pm   <$> parseField r 1 numeric) -- dittpName
        <*> (mdForwardRefOrNull cxt mt  <$> parseField r 2 numeric) -- dittpType
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoTemplateTypeParameter dittp)) pm

    26 -> label "METADATA_TEMPLATE_VALUE" $ do
      assertRecordSizeIn [5]
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      ditvp      <- DITemplateValueParameter
        <$> (                           parseField r 1 numeric) -- ditvpTag
        <*> (mdStringOrNull cxt pm  <$> parseField r 2 numeric) -- ditvpName
        <*> (mdForwardRefOrNull cxt mt <$> parseField r 3 numeric) -- ditvpName
        <*> (mdForwardRef cxt mt    <$> parseField r 4 numeric) -- ditvpValue
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoTemplateValueParameter ditvp)) pm

    27 -> label "METADATA_GLOBAL_VAR" $ do
      assertRecordSizeIn [11, 12]
      ctx        <- getContext
      field0     <- parseField r 0 numeric
      let isDistinct = testBit field0 0
          _version   = shiftR  field0 1 :: Int

      digv <- DIGlobalVariable
        <$> (mdForwardRefOrNull ctx mt <$> parseField r 1 numeric)  -- digvScope
        <*> (mdStringOrNull     ctx pm <$> parseField r 2 numeric)  -- digvName
        <*> (mdStringOrNull     ctx pm <$> parseField r 3 numeric)  -- digvLinkageName
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 4 numeric)  -- digvFile
        <*> parseField r 5 numeric                                  -- digvLine
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 6 numeric)  -- digvType
        <*> parseField r 7 nonzero                                  -- digvIsLocal
        <*> parseField r 8 nonzero                                  -- digvIsDefinition
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 9 numeric)  -- digvVariable
        <*> (mdForwardRefOrNull ctx mt <$> parseField r 10 numeric) -- digvDeclaration
        <*> if length (recordFields r) > 11
            then Just                  <$> parseField r 11 numeric  -- digvAlignment
            else pure Nothing
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoGlobalVariable digv)) pm

    28 -> label "METADATA_LOCAL_VAR" $ do
      -- this one is a bit funky:
      -- https://github.com/llvm-mirror/llvm/blob/release_38/lib/Bitcode/Reader/BitcodeReader.cpp#L2308
      assertRecordSizeBetween 8 10
      ctx    <- getContext
      field0 <- parseField r 0 numeric
      let isDistinct   = testBit (field0 :: Word32) 0
          hasAlignment = testBit (field0 :: Word32) 1

          hasTag | not hasAlignment && length (recordFields r) > 8 = 1
                 | otherwise                                       = 0

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
        <*> (mdStringOrNull     ("dilvName" :ctx) pm
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
      isDistinct <- parseField r 0 nonzero
      diExpr     <- DebugInfoExpression . DIExpression <$> parseFields r 1 numeric
      return $! updateMetadataTable (addDebugInfo isDistinct diExpr) pm

    30 -> label "METADATA_OBJC_PROPERTY" $ do
      -- TODO
      fail "not yet implemented"

    31 -> label "METADATA_IMPORTED_ENTITY" $ do
      assertRecordSizeIn [6, 7]
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      diie       <- DIImportedEntity
        <$> parseField r 1 numeric                                 -- diieTag
        <*> (mdForwardRefOrNull cxt mt <$> parseField r 2 numeric) -- diieScope
        <*> (mdForwardRefOrNull cxt mt <$> parseField r 3 numeric) -- diieEntity
        <*> (if length (recordFields r) >= 7
             then mdForwardRefOrNull cxt mt <$> parseField r 6 numeric
             else pure Nothing)                                    -- diieFile
        <*> parseField r 4 numeric                                 -- diieLine
        <*> (mdStringOrNull cxt pm     <$> parseField r 5 numeric) -- diieName

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
      assertRecordSizeIn [3]
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
      return $! addStrings strings pm

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
      assertRecordSizeIn [3]
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      digve      <- DIGlobalVariableExpression
        <$> (mdForwardRefOrNull cxt mt <$> parseField r 1 numeric) -- digveVariable
        <*> (mdForwardRefOrNull cxt mt <$> parseField r 2 numeric) -- digveExpression
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoGlobalVariableExpression digve)) pm

    38 -> label "METADATA_INDEX_OFFSET" $ do
      assertRecordSizeIn [2]
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
