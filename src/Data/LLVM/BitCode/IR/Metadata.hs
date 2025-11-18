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
  , parseDebugLoc
  , parseMetadataKindEntry
  , PartialUnnamedMd(..)
  , finalizePartialUnnamedMd
  , finalizePValMd
  , dedupMetadata
  , InstrMdAttachments
  , PFnMdAttachments
  , PKindMd
  , PGlobalAttachments
  , assertRecordSizeAtLeast
  , assertRecordSizeBetween
  , assertRecordSizeIn
  ) where

import           Data.LLVM.BitCode.Bitstream
import           Data.LLVM.BitCode.IR.Constants
import           Data.LLVM.BitCode.Match
import           Data.LLVM.BitCode.Parse
import           Data.LLVM.BitCode.Record
import           Text.LLVM.AST
import           Text.LLVM.Labels

import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import           Control.Applicative ((<|>))
import           Control.Exception (throw)
import           Control.Monad (foldM, guard, mplus, unless, when)
import           Data.Bits ( Bits, shiftR, testBit, shiftL, (.&.), (.|.), bit
                           , complement )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as Char8 (unpack)
import           Data.Data (Data)
import           Data.Either (partitionEithers)
import           Data.Generics.Uniplate.Data
import qualified Data.IntMap as IntMap
import           Data.List (mapAccumL, foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)
import           Data.Word (Word8,Word32,Word64)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack)
import           Data.Bifunctor (bimap)


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

-- | A variant of 'addDebugInfo' that only inserts the 'DebugInfo' into the
-- 'mtEntries', not the 'mtNodes'. This has the effect of causing the
-- 'DebugInfo' /not/ to be added to any top-level metadata lists and instead
-- causing it to be printed inline wherever it occurs.
-- See @Note [Printing metadata inline]@.
addInlineDebugInfo :: DebugInfo' Int -> MetadataTable -> MetadataTable
addInlineDebugInfo di mt = mt { mtEntries = addValue tv (mtEntries mt) }
  where
  tv = Typed { typedType  = PrimType Metadata
             , typedValue = ValMd (ValMdDebugInfo di)
             }

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
dedupMetadata :: Seq PartialUnnamedMd -> Seq PartialUnnamedMd
dedupMetadata pumd = helper (mkPartialUnnamedMdMap pumd) <$> pumd
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

        mkPartialUnnamedMdMap :: Seq PartialUnnamedMd -> Map PValMd Int
        mkPartialUnnamedMdMap =
          foldl' (\mp part -> Map.insert (pumValues part) (pumIndex part) mp) Map.empty

-- Finalizing ---------------------------------------------------------------

namedEntries :: PartialMetadata -> Seq NamedMd
namedEntries  = Seq.fromList
              . map (uncurry NamedMd)
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
finalizePValMd = relabel requireBbEntryName

-- | Partition unnamed entries into global and function local unnamed entries.
unnamedEntries :: PartialMetadata -> (Seq PartialUnnamedMd, Seq PartialUnnamedMd)
unnamedEntries pm = bimap Seq.fromList Seq.fromList (partitionEithers (mapMaybe resolveNode (IntMap.toList (mtNodes mt))))
  where
  mt = pmEntries pm

  -- TODO: is this silently eating errors with metadata that's not in the
  -- value table (when the lookupValueTableAbs fails)?
  resolveNode :: (Int, (Bool, Bool, Int))
              -> Maybe (Either PartialUnnamedMd PartialUnnamedMd)
  resolveNode (ref,(fnLocal,d,ix)) =
    ((if fnLocal then Right else Left) <$> lookupNode ref d ix)

  lookupNode :: Int -> Bool -> Int -> Maybe PartialUnnamedMd
  lookupNode ref d ix = do
    tv <- lookupValueTableAbs ref (mtEntries mt)
    case tv of
      Typed { typedValue = ValMd v } ->
        pure $! PartialUnnamedMd
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
  ( Seq NamedMd
  , (Seq PartialUnnamedMd, Seq PartialUnnamedMd)
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
  let -- Helpers for common patterns which appear below in parsing metadata
      ron n = do ctx <- getContext
                 mdForwardRefOrNull ctx mt <$> parseField r n numeric
      ronl n = if length (recordFields r) <= n then pure Nothing else ron n

  -- Note: the parsing cases below use a Monadic coding style, as opposed to an
  -- Applicative style (as was originally used) for performance reasons:
  -- Applicative record construction has quadratic size and corresponding
  -- performance impacts (the initial conversion from Applicative to Monadic
  -- saved 11s when parsing a 22MB bitcode file).
  --
  -- Additionally, this module uses RecordWildcards... a pragma that is not
  -- normally advisable but which does work to good effect in this situation to
  -- simplify the following and remove boilerplate intermediary assignments.

  in case recordCode r of
    -- [values]
    1 -> label "METADATA_STRING" $ do
      str <- fmap UTF8.decode (parseFields r 0 char) `mplus` parseField r 0 string
      return $! addString str pm

    -- [type num, value num]
    2 -> label "METADATA_VALUE" $ do
      assertRecordSizeIn r [2]
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
      cxt <- getContext
      isDistinct <- parseField r 0 nonzero
      loc <- parseDebugLoc 1
             (pure . mdForwardRef cxt mt)
             (pure . mdForwardRefOrNull cxt mt) r
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
      assertRecordSizeIn r [3, 5]
      field0 <- parseField r 0 unsigned
      let isDistinct = field0 .&. 0 == 1
      -- The format field determines what set of fields are contained in this
      -- record and what their types are (see
      -- https://github.com/llvm/llvm-project/blob/bbe8cd13/llvm/lib/Bitcode/Reader/MetadataLoader.cpp#L1437-L1444).
      let format = field0 `shiftR` 1
      let asValMdInt64 x = Just $ ValMdValue
                           $ Typed { typedType = PrimType $ Integer 64
                                   , typedValue = ValInteger x
                                   }
      diNode <- case format of
        2 -> do disrCount <- ron 1
                disrLowerBound <- ron 2
                disrUpperBound <- ron 3
                disrStride <- ron 4
                return $ DISubrange {..}
        1 -> do disrCount <- ron 1
                disrLowerBound <- ron 2
                let disrUpperBound = Nothing
                let disrStride = Nothing
                return $ DISubrange {..}
        0 -> do disrCount <- asValMdInt64 <$> parseField r 1 numeric
                disrLowerBound <- asValMdInt64 . fromIntegral <$> parseField r 2 signedInt64
                let disrUpperBound = Nothing
                let disrStride = Nothing
                return $ DISubrange {..}
        _ -> fail $ "Unknown format: " <> show format
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoSubrange diNode)) pm

    -- [isBigInt|isUnsigned|distinct, value, name]
    14 -> label "METADATA_ENUMERATOR" $ do
      assertRecordSizeAtLeast r 3
      ctx   <- getContext
      flags <- parseField r 0 numeric
      let isDistinct = testBit (flags :: Int) 0
          isUnsigned = testBit (flags :: Int) 1
          isBigInt   = testBit (flags :: Int) 2
      name  <- mdString ctx pm <$> parseField r 2 numeric
      value <-
        if isBigInt
          -- LLVM 12 or later
          then parseWideInteger r 3
          -- Pre-LLVM 12
          else toInteger <$> parseField r 1 signedInt64
      let diEnum = DebugInfoEnumerator name value isUnsigned
      return $! updateMetadataTable (addDebugInfo isDistinct diEnum) pm

    15 -> label "METADATA_BASIC_TYPE" $ do
      assertRecordSizeBetween r 6 8
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dibtTag <- parseField r 1 numeric
      dibtName <- mdString ctx pm <$> parseField r 2 numeric
      dibtSize <- parseField r 3 numeric
      dibtAlign <- parseField r 4 numeric
      dibtEncoding <- parseField r 5 numeric
      dibtFlags <- if length (recordFields r) <= 6
                   then pure Nothing
                   else Just <$> parseField r 6 numeric
      dibtNumExtraInhabitants <-
        if length (recordFields r) <= 7
        then pure 0
        else parseField r 7 numeric
      let dibt = DIBasicType {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoBasicType dibt)) pm

    -- [distinct, filename, directory]
    16 -> label "METADATA_FILE" $ do
      assertRecordSizeIn r [3, 5]
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      difFilename <- mdStringOrEmpty ctx pm <$> parseField r 1 numeric
      difDirectory <- mdStringOrEmpty ctx pm <$> parseField r 2 numeric
      let diFile = DIFile {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoFile diFile)) pm

    17 -> label "METADATA_DERIVED_TYPE" $ do
      -- While upstream LLVM currently imposes a maximum of 14 records per
      -- entry, we raise this to 15 for the sake of parsing Apple LLVM.
      -- See Note [Apple LLVM].
      assertRecordSizeBetween r 12 15
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      didtTag <- parseField r 1 numeric
      didtName <- mdStringOrNull ctx pm <$> parseField r 2 numeric
      didtFile <- ron 3
      didtLine <- parseField r 4 numeric
      didtScope <- ron 5
      didtBaseType <- ron 6
      didtSize <- parseField r 7 numeric
      didtAlign <- parseField r 8 numeric
      didtOffset <- parseField r 9 numeric
      didtFlags <- parseField r 10 numeric
      didtExtraData <- ron 11
      didtDwarfAddressSpace <-
        if length (recordFields r) <= 12
        then pure Nothing  -- field not present
        else do v <- parseField r 12 numeric
                -- dwarf address space is encoded in bitcode as +1; a value of
                -- zero means there is no dwarf address space present:
                -- https://github.com/llvm/llvm-project/blob/bbe8cd1/llvm/lib/Bitcode/Reader/MetadataLoader.cpp#L1544-L1548
                -- The AST representation is the actual address space, or Nothing
                -- if there is no address space (indistinguishable from "field
                -- not present" for LLVM 4 and earlier).
                if v == 0
                  then return Nothing
                  else return $ Just $ v - 1
      didtAnnotations <- if length (recordFields r) <= 13
                         then pure Nothing
                         else ron 13
      let didt = DIDerivedType {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoDerivedType didt)) pm

    18 -> label "METADATA_COMPOSITE_TYPE" $ do
      assertRecordSizeBetween r 16 26
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dictTag <- parseField r 1 numeric
      dictName <- mdStringOrNull ctx pm <$> parseField r 2 numeric
      dictFile <- ron 3
      dictLine <- parseField r 4 numeric
      dictScope <- ron 5
      dictBaseType <- ron 6
      dictSize <- parseField r 7 numeric
      dictAlign <- parseField r 8 numeric
      dictOffset <- parseField r 9 numeric
      dictFlags <- parseField r 10 numeric
      dictElements <- ron 11
      dictRuntimeLang <- parseField r 12 numeric
      dictVTableHolder <- ron 13
      dictTemplateParams <- ron 14
      dictIdentifier <- mdStringOrNull ctx pm <$> parseField r 15 numeric
      dictDiscriminator <- ronl 16
      dictDataLocation <- ronl 17
      dictAssociated <- ronl 18
      dictAllocated <- ronl 19
      dictRank <- ronl 20
      dictAnnotations <- ronl 21
      dictNumExtraInhabitants <- if length (recordFields r) <= 22
                                 then pure 0
                                 else parseField r 22 numeric
      dictSpecification <- ronl 23
      dictEnumKind <- if length (recordFields r) <= 24
                      then pure Nothing
                      else do f <- parseField r 24 numeric
                              if f == dwarf_DW_APPLE_ENUM_KIND_invalid
                                then pure Nothing
                                else pure $ Just f
      dictBitStride <- ronl 25
      let dict = DICompositeType {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoCompositeType dict)) pm

    19 -> label "METADATA_SUBROUTINE_TYPE" $ do
      assertRecordSizeBetween r 3 4
      isDistinct <- parseField r 0 nonzero
      distFlags <- parseField r 1 numeric
      distTypeArray <- ron 2
      let dist = DISubroutineType {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoSubroutineType dist)) pm

    20 -> label "METADATA_COMPILE_UNIT" $ do
      assertRecordSizeBetween r 14 22
      let recordSize = length (recordFields r)
      ctx        <- getContext
      isDistinct <- parseField r 0 nonzero
      dicuLanguage <- parseField r 1 numeric
      dicuFile <- ron 2
      dicuProducer <- mdStringOrNull ctx pm <$> parseField r 3 numeric
      dicuIsOptimized <- parseField r 4 nonzero
      dicuFlags <- mdStringOrNull ctx pm <$> parseField r 5 numeric
      dicuRuntimeVersion <- parseField r 6 numeric
      dicuSplitDebugFilename <- mdStringOrNull ctx pm <$> parseField r 7 numeric
      dicuEmissionKind <- parseField r 8 numeric
      dicuEnums <- ron 9
      dicuRetainedTypes <- ron 10
      dicuSubprograms <- ron 11
      dicuGlobals <- ron 12
      dicuImports <- ron 13
      dicuMacros <- if recordSize <= 15
                    then pure Nothing
                    else ron 15
      dicuDWOId <- if recordSize <= 14
                   then pure 0
                   else parseField r 14 numeric
      dicuSplitDebugInlining <- if recordSize <= 16
                                then pure True
                                else parseField r 16 nonzero
      dicuDebugInfoForProf <- if recordSize <= 17
                              then pure False
                              else parseField r 17 nonzero
      dicuNameTableKind <- if recordSize <= 18
                           then pure 0
                           else parseField r 18 numeric
      dicuRangesBaseAddress <- if recordSize <= 19
                               then pure False
                               else parseField r 19 nonzero
      dicuSysRoot <- if recordSize <= 20
                     then pure Nothing
                     else mdStringOrNull ctx pm <$> parseField r 20 numeric
      dicuSDK <- if recordSize <= 21
                 then pure Nothing
                 else mdStringOrNull ctx pm <$> parseField r 21 numeric
      let dicu = DICompileUnit {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoCompileUnit dicu)) pm


    21 -> label "METADATA_SUBPROGRAM" $ do
      -- this one is a bit funky:
      -- https://github.com/llvm/llvm-project/blob/release/10.x/llvm/lib/Bitcode/Reader/MetadataLoader.cpp#L1486

      assertRecordSizeBetween r 18 21
      -- A "version" is encoded in the high-order bits of the isDistinct field.
      version <- parseField r 0 numeric

      let hasSPFlags = (version .&. (0x4 :: Word64)) /= 0;

      (diFlags0, spFlags0) <-
        if hasSPFlags then
          (,) <$> parseField r 11 numeric <*> parseField r 9 numeric
        else
          (,) <$> parseField r (11 + 2) numeric <*> pure 0

      let diFlagMainSubprogram = bit 21 :: Word32
          hasOldMainSubprogramFlag = (diFlags0 .&. diFlagMainSubprogram) /= 0

          -- CF https://github.com/llvm/llvm-project/blob/release/10.x/llvm/include/llvm/IR/DebugInfoFlags.def
          spFlagIsLocal      = bit 2
          spFlagIsDefinition = bit 3
          spFlagIsOptimized  = bit 4
          spFlagIsMain       = bit 8

          dispFlags :: Word32
          dispFlags
            | hasOldMainSubprogramFlag = diFlags0 .&. complement diFlagMainSubprogram
            | otherwise                = diFlags0

          spFlags :: Word32
          spFlags
            | hasOldMainSubprogramFlag = spFlags0 .|. spFlagIsMain
            | otherwise                = spFlags0

      -- TODO, isMain isn't exposed via DISubprogram
      (dispIsLocal, dispIsDefinition, dispIsOptimized, dispVirtuality, _isMain) <-
        if hasSPFlags then
          let spIsLocal       = spFlags .&. spFlagIsLocal /= 0
              spIsDefinition  = spFlags .&. spFlagIsDefinition /= 0
              spIsOptimized   = spFlags .&. spFlagIsOptimized /= 0
              spIsMain        = spFlags .&. spFlagIsMain /= 0
              spVirtuality :: Word8
              spVirtuality    = fromIntegral (spFlags .&. 0x3)
           in return (spIsLocal, spIsDefinition, spIsOptimized, spVirtuality, spIsMain)
        else
          do spIsLocal <- parseField r 7 nonzero
             spIsDefinition <- parseField r 8 nonzero
             spIsOptimized <- parseField r 14 nonzero
             spVirtuality <- parseField r 11 numeric
             return (spIsLocal, spIsDefinition, spIsOptimized, spVirtuality, hasOldMainSubprogramFlag)

      let recordSize = length (recordFields r)

          isDistinct = (version .&. 0x1 /= 0) || (spFlags .&. spFlagIsDefinition /= 0)

          hasUnit = version .&. 0x2 /= 0

          offsetA
            | not hasSPFlags = 2
            | otherwise      = 0

          offsetB
            | not hasSPFlags && recordSize >= 19 = 3
            | not hasSPFlags                     = 2
            | otherwise                          = 0

          -- this doesn't seem to be used in our parser...
          --hasFn
          --  | not hasSPFlags && recordSize >= 19 = not hasUnit
          --  | otherwise = False

          hasThisAdjustment
            | not hasSPFlags = recordSize >= 20
            | otherwise      = True

          hasThrownTypes
            | not hasSPFlags = recordSize >= 21
            | otherwise      = True

          hasAnnotations
            | not hasSPFlags = False
            | otherwise      = recordSize >= 19

      -- Some additional sanity checking
      when (not hasSPFlags && hasUnit)
           (assertRecordSizeBetween r 19 21)

      when (hasSPFlags && not hasUnit)
           (fail "DISubprogram record has subprogram flags, but does not have unit.  Invalid record.")

      ctx <- getContext

      -- Forward references that depend on the 'version'
      let optFwdRef b n =
            if b
            then mdForwardRefOrNull ctx mt <$> parseField r n numeric
            else pure Nothing

      dispScope <- ron 1
      dispName <- mdStringOrNull ctx pm <$> parseField r 2 numeric
      dispLinkageName <- mdStringOrNull ctx pm <$> parseField r 3 numeric
      dispFile <- ron 4
      dispLine <- parseField r 5 numeric
      dispType <- ron 6
      dispScopeLine <- parseField r (7 + offsetA) numeric
      dispContainingType <- ron (8 + offsetA)
      dispVirtualIndex <- parseField r (10 + offsetA) numeric
      dispThisAdjustment <- if hasThisAdjustment
                            then parseField r (16 + offsetB) numeric
                            else return 0
      dispUnit <- optFwdRef hasUnit (12 + offsetB)
      dispTemplateParams <- ron (13 + offsetB)
      dispDeclaration <- ron (14 + offsetB)
      dispRetainedNodes <- ron (15 + offsetB)
      dispThrownTypes <- optFwdRef hasThrownTypes (17 + offsetB)
      dispAnnotations <- optFwdRef hasAnnotations (18 + offsetB)

      let disp = DISubprogram {..}

      -- TODO: in the LLVM parser, it then goes into the metadata table
      -- and updates function entries to point to subprograms. Is that
      -- neccessary for us?
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoSubprogram disp)) pm

    22 -> label "METADATA_LEXICAL_BLOCK" $ do
      assertRecordSizeIn r [5]
      isDistinct <- parseField r 0 nonzero
      dilbScope <- ron 1
      dilbFile <- ron 2
      dilbLine <- parseField r 3 numeric
      dilbColumn <- parseField r 4 numeric
      let dilb = DILexicalBlock {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoLexicalBlock dilb)) pm

    23 -> label "METADATA_LEXICAL_BLOCK_FILE" $ do
      assertRecordSizeIn r [4]
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      dilbfScope <- mdForwardRef cxt mt <$> parseField r 1 numeric
      dilbfFile <- ron 2
      dilbfDiscriminator <- parseField r 3 numeric
      let dilbf = DILexicalBlockFile {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoLexicalBlockFile dilbf)) pm

    24 -> label "METADATA_NAMESPACE" $ do
      assertRecordSizeIn r [3, 5]
      let isNew = length (recordFields r) == 3
      let nameIdx = if isNew then 2 else 3

      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      dinsName <- mdStringOrNull cxt pm <$> parseField r nameIdx numeric
      dinsScope <- mdForwardRef cxt mt <$> parseField r 1 numeric
      dinsFile <- if isNew
                  then return (ValMdString "")
                  else mdForwardRef cxt mt <$> parseField r 2 numeric
      dinsLine <- if isNew then return 0 else parseField r 4 numeric
      let dins = DINameSpace {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoNameSpace dins)) pm

    25 -> label "METADATA_TEMPLATE_TYPE" $ do
      assertRecordSizeIn r [3, 4]
      let hasIsDefault = length (recordFields r) == 4
      cxt <- getContext
      isDistinct <- parseField r 0 nonzero
      dittpName <- mdStringOrNull cxt pm <$> parseField r 1 numeric
      dittpType <- ron 2
      dittpIsDefault <- if hasIsDefault
                        then Just <$> parseField r 3 boolean
                        else pure Nothing
      let dittp = DITemplateTypeParameter {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoTemplateTypeParameter dittp)) pm

    26 -> label "METADATA_TEMPLATE_VALUE" $ do
      assertRecordSizeIn r [5, 6]
      let hasIsDefault = length (recordFields r) == 6
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      ditvpTag <- parseField r 1 numeric
      ditvpName <- mdStringOrNull cxt pm <$> parseField r 2 numeric
      ditvpType <- ron 3
      ditvpIsDefault <- if hasIsDefault
                        then Just <$> parseField r 4 boolean
                        else pure Nothing
      ditvpValue <- mdForwardRef cxt mt <$> parseField r (if hasIsDefault then 5 else 4) numeric
      let ditvp = DITemplateValueParameter {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoTemplateValueParameter ditvp)) pm

    27 -> label "METADATA_GLOBAL_VAR" $ do
      assertRecordSizeBetween r 11 13
      ctx        <- getContext
      field0     <- parseField r 0 numeric
      let isDistinct = testBit field0 0
          _version   = shiftR  field0 1 :: Int

      digvScope <- ron 1
      digvName <- mdStringOrNull ctx pm <$> parseField r 2 numeric
      digvLinkageName <- mdStringOrNull ctx pm <$> parseField r 3 numeric
      digvFile <- ron 4
      digvLine <- parseField r 5 numeric
      digvType <- ron 6
      digvIsLocal <- parseField r 7 nonzero
      digvIsDefinition <- parseField r 8 nonzero
      digvVariable <- ron 9
      digvDeclaration <- ron 10
      digvAlignment <- if length (recordFields r) > 11
                       then Just <$> parseField r 11 numeric
                       else pure Nothing
      digvAnnotations <- if length (recordFields r) > 12
                         then ron 12
                         else pure Nothing
      let digv = DIGlobalVariable {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoGlobalVariable digv)) pm

    28 -> label "METADATA_LOCAL_VAR" $ do
      -- this one is a bit funky:
      -- https://github.com/llvm-mirror/llvm/blob/release_38/lib/Bitcode/Reader/BitcodeReader.cpp#L2308
      assertRecordSizeBetween r 8 10
      ctx    <- getContext
      field0 <- parseField r 0 numeric
      let isDistinct   = testBit (field0 :: Word32) 0
          hasAlignment = testBit (field0 :: Word32) 1

          hasTag | not hasAlignment && length (recordFields r) > 8 = 1
                 | otherwise                                       = 0

          adj i = i + hasTag


      dilvScope <- mdForwardRefOrNull ("dilvScope":ctx) mt
                   <$> parseField r (adj 1) numeric
      dilvName <- mdStringOrNull     ("dilvName" :ctx) pm
                  <$> parseField r (adj 2) numeric
      dilvFile <- mdForwardRefOrNull ("dilvFile" :ctx) mt
                  <$> parseField r (adj 3) numeric
      dilvLine <- parseField r (adj 4) numeric
      dilvType <- mdForwardRefOrNull ("dilvType" :ctx) mt
                  <$> parseField r (adj 5) numeric
      dilvArg <- parseField r (adj 6) numeric
      dilvFlags <- parseField r (adj 7) numeric
      dilvAlignment <-
        if hasAlignment
          then do n <- parseField r 8 numeric
                  when ((n :: Word64) > fromIntegral (maxBound :: Word32))
                        (fail "Alignment value is too large")
                  return $ Just (fromIntegral n :: Word32)
          else return Nothing
      dilvAnnotations <- if hasAlignment && length (recordFields r) > 9
                          then ron 9
                          else pure Nothing
      let dilv = DILocalVariable {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoLocalVariable dilv)) pm

    29 -> label "METADATA_EXPRESSION" $ do
      {-
      Although DIExpressions store an `isDistinct` field in LLVM bitcode, it is
      never used in practice. This is because DIExpressions are always printed
      inline in definitions, and since the `distinct` keyword is only printed in
      top-level metadata lists, there is no way for `distinct` to be printed
      before a DIExpression. See also Note [Printing metadata inline].
      -}
      -- isDistinct <- parseField r 0 nonzero
      diExpr     <- DebugInfoExpression . DIExpression <$> parseFields r 1 numeric
      return $! updateMetadataTable (addInlineDebugInfo diExpr) pm

    30 -> label "METADATA_OBJC_PROPERTY" $ do
      -- TODO
      fail "not yet implemented"

    31 -> label "METADATA_IMPORTED_ENTITY" $ do
      assertRecordSizeIn r [6, 7]
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      diieTag <- parseField r 1 numeric
      diieScope <- ron 2
      diieEntity <- ron 3
      diieFile <- if length (recordFields r) >= 7
                  then ron 6
                  else pure Nothing
      diieLine <- parseField r 4 numeric
      diieName <- mdStringOrNull cxt pm <$> parseField r 5 numeric
      let diie = DIImportedEntity {..}
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
      assertRecordSizeIn r [3]
      count  <- parseField r 0 numeric
      offset <- parseField r 1 numeric
      bs     <- parseField r 2 fieldBlob
      when (count == 0)
        (fail "Invalid record: metadata strings with no strings")
      when (offset > S.length bs)
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
      assertRecordSizeIn r [3]
      isDistinct <- parseField r 0 nonzero
      digveVariable <- ron 1
      digveExpression <- ron 2
      let digve = DIGlobalVariableExpression {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoGlobalVariableExpression digve)) pm

    38 -> label "METADATA_INDEX_OFFSET" $ do
      assertRecordSizeIn r [2]
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

    40 -> label "METADATA_LABEL" $ do
      assertRecordSizeIn r [5]
      cxt        <- getContext
      isDistinct <- parseField r 0 nonzero
      dilScope <- ron 1
      dilName <- mdString cxt pm <$> parseField r 2 numeric
      dilFile <- ron 3
      dilLine <- parseField r 4 numeric
      let dil = DILabel {..}
      return $! updateMetadataTable
        (addDebugInfo isDistinct (DebugInfoLabel dil)) pm

    41 -> label "METADATA_STRING_TYPE" $ do
      notImplemented

    -- Codes 42 and 43 are reserved for Fortran array–specific debug info, see
    -- https://github.com/llvm/llvm-project/blob/4681f6111e655057f5015564a9bf3705f87495bf/llvm/include/llvm/Bitcode/LLVMBitCodes.h#L348-L349

    44 -> label "METADATA_COMMON_BLOCK" $ do
      notImplemented

    45 -> label "METADATA_GENERIC_SUBRANGE" $ do
      notImplemented

    46 -> label "METADATA_ARG_LIST" $ do
      cxt <- getContext
      dial <- DIArgList
        <$> (map (mdForwardRef cxt mt) <$> parseFields r 0 numeric)
      return $! updateMetadataTable
        (addInlineDebugInfo (DebugInfoArgList dial)) pm

    47 -> label "METADATA_ASSIGN_ID" $ do
      assertRecordSizeIn r [1]
      isDistinct <- parseField r 0 nonzero
      -- Inspirted by a similar check in
      -- https://github.com/llvm/llvm-project/blob/7a0b9daac9edde4293d2e9fdf30d8b35c04d16a6/llvm/lib/Bitcode/Reader/MetadataLoader.cpp#L2071-L2072
      unless isDistinct $
        fail "Invalid DIAssignID record. Must be distinct"
      return $! updateMetadataTable
        (addDebugInfo isDistinct DebugInfoAssignID) pm

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

parseDebugLoc :: Num a => Bits a => Num b => Bits b
              => Int
              -> (a -> Parse (ValMd' r))
              -> (b -> Parse (Maybe (ValMd' r))) -> Record
              -> Parse (DebugLoc' r)
parseDebugLoc idx resolveScope resolveIA r = do
  assertRecordSizeIn r [ idx + i | i <- [4, 5, 7] ]
  let recordSize = length $ recordFields r
  let field = parseField r . (idx +)
  let fieldDef d i = if recordSize < (idx + 1 + i)
                     then const (pure d)
                     else field i
  dlLine <- field 0 numeric
  dlCol <- field 1 numeric
  dlScope <- resolveScope =<< field 2 numeric
  dlIA <- resolveIA =<< field 3 numeric
  dlImplicit <- fieldDef False 4 nonzero
  return DebugLoc {..}


parseMetadataKindEntry :: Record -> Parse ()
parseMetadataKindEntry r = do
  kind <- parseField  r 0 numeric
  name <- parseFields r 1 char
  addKind kind (UTF8.decode name)

----------------------------------------------------------------------

-- The assert* functions below check if a metadata record size matches
-- what is expected, and if not, emit a warning.
--
-- In the past, we made these fatal errors instead of warnings, but we
-- downgraded them to a warning due to how frequently LLVM adds new
-- metadata record fields. Moreover, it is usually not a serious problem
-- for downstream users that llvm-pretty-bc-parser lacks newer metadata
-- fields, since they usually do not affect the semantics of the overall
-- LLVM bitcode.

assertRecordSizeBetween :: Record -> Int -> Int -> Parse ()
assertRecordSizeBetween r lb ub = do
  cxt <- getContext
  let len = length (recordFields r)
  when (len < lb || ub < len) $
    addParseWarning $
    InvalidMetadataRecordSize len (MetadataRecordSizeBetween lb ub) cxt

assertRecordSizeIn :: Record -> [Int] -> Parse ()
assertRecordSizeIn r ns = do
  cxt <- getContext
  let len = length (recordFields r)
  when (not (len `elem` ns)) $
    addParseWarning $
    InvalidMetadataRecordSize len (MetadataRecordSizeIn ns) cxt

assertRecordSizeAtLeast :: Record -> Int -> Parse ()
assertRecordSizeAtLeast r lb = do
  cxt <- getContext
  let len = length (recordFields r)
  when (len < lb) $
    addParseWarning $
    InvalidMetadataRecordSize len (MetadataRecordSizeAtLeast lb) cxt


----------------------------------------------------------------------

{-
Note [Apple LLVM]
~~~~~~~~~~~~~~~~~
Apple maintains a fork of LLVM, whose source code can be found at
https://github.com/apple/llvm-project. The version of Clang that is shipped
with Xcode, and thereby the de facto default Clang version on macOS, is based
on this LLVM fork. To distinguish between the two LLVM codebases, we will refer
to "upstream LLVM" and "Apple LLVM" throughout this Note.

One of the more noticeable differences between upstream and Apple LLVM is that
Apple LLVM uses a slightly different bitcode format. In particular, Apple LLVM
has support for pointer authentication
(https://lists.llvm.org/pipermail/llvm-dev/2019-October/136091.html), which
requires adding an extra record to the METADATA_DERIVED_TYPE entry that is not
present in upstream LLVM. This impacts llvm-pretty-bc-parser, as we currently
check that the number of records does not exceed a certain maximum, but this
maximum is different depending on whether we parse upstream or Apple LLVM
bitcode.

For now, we work around this issue by raising the maximum number of
METADATA_DERIVED_TYPE records by one to accommodate Apple LLVM, but we do not
actually parse any information related to pointer authentication. This should
work provided that Apple LLVM continues to encode pointer authentication–related
metadata in the same part of METADATA_DERIVED_TYPE in future releases. If this
assumption does not hold true in the future, we will likely need a more
sophisticated solution that involves parsing the bitcode differently depending
on what Apple LLVM version was used to produce a bitcode file.

Note [Printing metadata inline]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are some forms of metadata that we should always print inline and never
create entries for in top-level metadata lists (named or otherwise). Currently,
these forms of metadata are:

* DIExpression
* DIArgList

This list is taken from the LLVM source code here:
https://github.com/llvm/llvm-project/blob/65600cb2a7e940babf6c493503b9d3fd19f8cb06/llvm/lib/IR/AsmWriter.cpp#L1242-L1245

Implementation-wise, this is accomplished by using `addInlineDebugInfo`. Unlike
`addDebugInfo`, this inserts the metadata field into a separate `mtEntries` map
that is not used to populate top-level metadata lists when pretty-printing an
LLVM module.
-}
