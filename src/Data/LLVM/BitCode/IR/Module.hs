{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Data.LLVM.BitCode.IR.Module where

import Data.LLVM.BitCode.Bitstream
import Data.LLVM.BitCode.IR.Attrs
import Data.LLVM.BitCode.IR.Blocks
import Data.LLVM.BitCode.IR.Constants
import Data.LLVM.BitCode.IR.Function
import Data.LLVM.BitCode.IR.Globals
import Data.LLVM.BitCode.IR.Metadata
import Data.LLVM.BitCode.IR.Types
import Data.LLVM.BitCode.IR.Values
import Data.LLVM.BitCode.Match
import Data.LLVM.BitCode.Parse
import Data.LLVM.BitCode.Record
import Text.LLVM.AST

import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import Control.Monad (foldM,guard,when,forM_)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T


-- Module Block Parsing --------------------------------------------------------

data PartialModule = PartialModule
  { partialGlobalIx   :: !Int
  , partialGlobals    :: !GlobalList
  , partialDefines    :: DefineList
  , partialDeclares   :: DeclareList
  , partialDataLayout :: DataLayout
  , partialInlineAsm  :: InlineAsm
  , partialComdat     :: Seq.Seq (String,SelectionKind)
  , partialAliasIx    :: !Int
  , partialAliases    :: AliasList
  , partialNamedMd    :: [NamedMd]
  , partialUnnamedMd  :: [PartialUnnamedMd]
  , partialSections   :: Seq.Seq String
  , partialSourceName :: !(Maybe String)
  }

emptyPartialModule :: PartialModule
emptyPartialModule  = PartialModule
  { partialGlobalIx   = 0
  , partialGlobals    = mempty
  , partialDefines    = mempty
  , partialDeclares   = mempty
  , partialDataLayout = mempty
  , partialInlineAsm  = mempty
  , partialAliasIx    = 0
  , partialAliases    = mempty
  , partialNamedMd    = mempty
  , partialUnnamedMd  = mempty
  , partialSections   = mempty
  , partialSourceName = mempty
  , partialComdat     = mempty
  }

-- | Fixup the global variables and declarations, and return the completed
-- module.
finalizeModule :: PartialModule -> Parse Module
finalizeModule pm = label "finalizeModule" $ do
  globals  <- T.mapM finalizeGlobal       (partialGlobals pm)
  declares <- T.mapM finalizeDeclare      (partialDeclares pm)
  aliases  <- T.mapM finalizePartialAlias (partialAliases pm)
  unnamed  <- T.mapM finalizePartialUnnamedMd (partialUnnamedMd pm)
  types    <- resolveTypeDecls
  let lkp = lookupBlockName (partialDefines pm)
  defines <- T.mapM (finalizePartialDefine lkp) (partialDefines pm)
  return emptyModule
    { modSourceName = partialSourceName pm
    , modDataLayout = partialDataLayout pm
    , modNamedMd    = partialNamedMd pm
    , modUnnamedMd  = sortBy (comparing umIndex) unnamed
    , modGlobals    = F.toList globals
    , modDefines    = F.toList defines
    , modTypes      = types
    , modDeclares   = F.toList declares
    , modInlineAsm  = partialInlineAsm pm
    , modAliases    = F.toList aliases
    , modComdat     = Map.fromList (F.toList (partialComdat pm))
    }

-- | Parse an LLVM Module out of the top-level block in a Bitstream.
parseModuleBlock :: [Entry] -> Parse Module
parseModuleBlock ents = label "MODULE_BLOCK" $ do

  -- the explicit type symbol table has been removed in 3.1, so we parse the
  -- type table, and generate the type symbol table from it.
  tsymtab <- label "type symbol table" $ do
    mb <- match (findMatch typeBlockIdNew) ents
    case mb of
      Just es -> parseTypeBlock es
      Nothing -> return mempty

  withTypeSymtab tsymtab $ label "value symbol table" $ do
    -- parse the value symbol table out first, if there is one
    symtab <- do
      mb <- match (findMatch valueSymtabBlockId) ents
      case mb of
        Just es -> parseValueSymbolTableBlock es
        Nothing -> return emptyValueSymtab

    withValueSymtab symtab $ do

      -- Before parsing global variables, we have to get the module code version.
      _ <- findModuleCodeEntry ents

      -- Global variables contain no references to other parts of the AST
      -- but are themselves referenced. Thus, they are parsed first.
      pm <- parseGlobalVars emptyPartialModule ents

      finalizeModule =<< foldM parseModuleBlockEntry pm ents

-- | See: http://llvm.org/docs/BitCodeFormat.html#module-code-version-record
findModuleCodeEntry :: [Entry] -> Parse ()
findModuleCodeEntry ents = label "MODULE_CODE_VERSION" $ do
  mb <- match (findMatch moduleCodeVersion) ents
  case mb of
    Nothing  -> fail "No module code version found!" -- TODO: is it always present?
    Just mcv -> do
      version <- parseField mcv 0 numeric
      setModVersion version
      case version :: Int of
        0 -> setRelIds False  -- Absolute value ids in LLVM <= 3.2
        1 -> setRelIds True   -- Relative value ids in LLVM >= 3.3
        2 -> setRelIds True   -- Relative value ids in LLVM >= 5.0
        _ -> fail ("unsupported version id: " ++ show version)

-- | Parse all entries containing global variables
parseGlobalVars :: PartialModule -> [Entry] -> Parse PartialModule
parseGlobalVars pm ents = label "global variables" $ do
  let foldM' acc l f = foldM f acc l
  foldM' pm (mapMaybe moduleCodeGlobalvar ents) $ \pm r -> do
    pg <- parseGlobalVar (partialGlobalIx pm) r
    return pm { partialGlobalIx = succ $! (partialGlobalIx pm)
              , partialGlobals  = partialGlobals pm Seq.|> pg
              }

-- | Parse the entries in a module block.
parseModuleBlockEntry :: PartialModule -> Entry -> Parse PartialModule

parseModuleBlockEntry pm (blockInfoBlockId -> Just _) =
  -- skip the block info block, as we only use it during Bitstream parsing.
  return pm

parseModuleBlockEntry pm (typeBlockIdNew -> Just _) = do
  -- TYPE_BLOCK_ID_NEW
  -- this is skipped, as it's parsed before anything else, in parseModuleBlock
  return pm

parseModuleBlockEntry pm (constantsBlockId -> Just es) = do
  -- CONSTANTS_BLOCK_ID
  parseConstantsBlock es
  return pm

parseModuleBlockEntry pm (moduleCodeFunction -> Just r) = do
  -- MODULE_CODE_FUNCTION
  parseFunProto r pm

parseModuleBlockEntry pm (functionBlockId -> Just es) = label "FUNCTION_BLOCK_ID" $ do
  let unnamedGlobalsCount = length (partialUnnamedMd pm)
  def <- parseFunctionBlock unnamedGlobalsCount es
  let def' = def { partialGlobalMd = [] }
  return pm { partialDefines = partialDefines pm Seq.|> def'
            , partialUnnamedMd = partialGlobalMd def ++ partialUnnamedMd pm
            }

parseModuleBlockEntry pm (paramattrBlockId -> Just _) = do
  -- PARAMATTR_BLOCK_ID
  -- TODO: skip for now
  return pm

parseModuleBlockEntry pm (paramattrGroupBlockId -> Just _) = do
  -- PARAMATTR_GROUP_BLOCK_ID
  -- TODO: skip for now
  return pm

parseModuleBlockEntry pm (metadataBlockId -> Just es) = label "METADATA_BLOCK_ID" $ do
  vt <- getValueTable
  let globalsSoFar = length (partialUnnamedMd pm)
  (ns,(gs,_),_,_,atts) <- parseMetadataBlock globalsSoFar vt es
  return $ addGlobalAttachments atts pm
    { partialNamedMd   = partialNamedMd   pm ++ ns
    , partialUnnamedMd = partialUnnamedMd pm ++ gs
    }

parseModuleBlockEntry pm (valueSymtabBlockId -> Just _es) = do
  -- VALUE_SYMTAB_BLOCK_ID
  -- NOTE: we parse the value symbol table eagerly at the beginning of the
  -- MODULE_BLOCK
  return pm

parseModuleBlockEntry pm (moduleCodeTriple -> Just _) = do
  -- MODULE_CODE_TRIPLE
  return pm

parseModuleBlockEntry pm (moduleCodeDatalayout -> Just r) = do
  -- MODULE_CODE_DATALAYOUT
  layout <- UTF8.decode <$> parseFields r 0 char
  case parseDataLayout layout of
    Nothing -> fail ("unable to parse data layout: ``" ++ layout ++ "''")
    Just dl -> return (pm { partialDataLayout = dl })

parseModuleBlockEntry pm (moduleCodeAsm -> Just r) = do
  -- MODULE_CODE_ASM
  asm <- UTF8.decode <$> parseFields r 0 char
  return pm { partialInlineAsm = lines asm }

parseModuleBlockEntry pm (abbrevDef -> Just _) = do
  -- skip abbreviation definitions
  return pm

parseModuleBlockEntry pm (moduleCodeGlobalvar -> Just _) = do
  -- MODULE_CODE_GLOBALVAR
  -- These are parsed before the rest of the module, they can be ignored
  return pm

parseModuleBlockEntry pm (moduleCodeAlias -> Just r) = label "MODULE_CODE_ALIAS_OLD" $ do
  pa <- parseAliasOld (partialAliasIx pm) r
  return pm
    { partialAliasIx = succ (partialAliasIx pm)
    , partialAliases = partialAliases pm Seq.|> pa
    }

parseModuleBlockEntry pm (moduleCodeVersion -> Just _) = do
  -- MODULE_CODE_VERSION
  -- Already handled
  return pm

parseModuleBlockEntry pm (moduleCodeSectionname -> Just r) = do
  name <- UTF8.decode <$> parseFields r 0 char
  return pm { partialSections = partialSections pm Seq.|> name }

parseModuleBlockEntry pm (moduleCodeComdat -> Just r) = do
  -- MODULE_CODE_COMDAT
  when (length (recordFields r) < 2) (fail "Invalid record (MODULE_CODE_COMDAT)")
  v <- getModVersion
  (kindVal, name) <- if | v >= 2 -> do
                            kindVal <- parseField r 0 numeric
                            name <- UTF8.decode <$> parseFields r 2 char
                            return (kindVal, name)
                        | otherwise -> do
                            offset <- parseField r 0 numeric
                            len <- parseField r 1 numeric
                            kindVal <- parseField r 2 numeric
                            mst <- getStringTable
                            let msg = "No string table for new-style COMDAT."
                            st <- maybe (fail msg) return mst
                            let Symbol name = resolveStrtabSymbol st offset len
                            return (kindVal, name)
  kind <- case kindVal :: Int of
            1  -> pure ComdatAny
            2  -> pure ComdatExactMatch
            3  -> pure ComdatLargest
            4  -> pure ComdatNoDuplicates
            5  -> pure ComdatSameSize
            _  -> pure ComdatAny -- This matches the C++ code as of 7d085b607d
  return pm { partialComdat = partialComdat pm Seq.|> (name,kind) }

parseModuleBlockEntry pm (moduleCodeVSTOffset -> Just _) = do
  -- MODULE_CODE_VSTOFFSET
  -- TODO: should we handle this?
  return pm

parseModuleBlockEntry pm (moduleCodeAliasNew -> Just r) = label "MODULE_CODE_ALIAS" $ do
  pa <- parseAlias r
  return pm
    { partialAliasIx = succ (partialAliasIx pm)
    , partialAliases = partialAliases pm Seq.|> pa
    }

parseModuleBlockEntry pm (moduleCodeMDValsUnused -> Just _) = do
  -- MODULE_CODE_METADATA_VALUES_UNUSED
  return pm

parseModuleBlockEntry pm (moduleCodeSourceFilename -> Just r) = do
  -- MODULE_CODE_SOURCE_FILENAME
  do str <- parseField r 0 cstring
     return pm { partialSourceName = Just str }

parseModuleBlockEntry pm (moduleCodeHash -> Just _) = do
  -- MODULE_CODE_HASH
  -- It should be safe to ignore this for now.
  --fail "MODULE_CODE_HASH"
  return pm

parseModuleBlockEntry _ (moduleCodeIFunc -> Just _) = do
  -- MODULE_CODE_IFUNC
  fail "MODULE_CODE_IFUNC"

parseModuleBlockEntry pm (uselistBlockId -> Just _) = do
  -- USELIST_BLOCK_ID
  -- XXX ?? fail "USELIST_BLOCK_ID"
  return pm

parseModuleBlockEntry _ (moduleStrtabBlockId -> Just _) = do
  -- MODULE_STRTAB_BLOCK_ID
  fail "MODULE_STRTAB_BLOCK_ID"

parseModuleBlockEntry pm (globalvalSummaryBlockId -> Just _) = do
  -- GLOBALVAL_SUMMARY_BLOCK_ID
  -- It should be safe to ignore this for now.
  return pm

parseModuleBlockEntry pm (operandBundleTagsBlockId -> Just _) = do
  -- OPERAND_BUNDLE_TAGS_BLOCK_ID
  -- fail "OPERAND_BUNDLE_TAGS_BLOCK_ID"
  return pm

parseModuleBlockEntry pm (metadataKindBlockId -> Just es) = label "METADATA_KIND_BLOCK_ID" $ do
  forM_ es $ \e ->
    case fromEntry e of
      Just r -> parseMetadataKindEntry r
      Nothing -> fail "Can't parse metadata kind block entry."
  return pm

parseModuleBlockEntry pm (strtabBlockId -> Just _) =
  -- Handled already.
  return pm

parseModuleBlockEntry pm (ltoSummaryBlockId -> Just _) =
  -- It should be safe to ignore this for now.
  --label "FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID" $ do
  --  fail "FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID unsupported"
  return pm

parseModuleBlockEntry pm (symtabBlockId -> Just [symtabBlobId -> Just _]) =
  -- Handled already
  return pm

parseModuleBlockEntry pm (syncScopeNamesBlockId -> Just _) =
  label "SYNC_SCOPE_NAMES_BLOCK_ID" $ do
    -- TODO: record this information somewhere
    return pm

parseModuleBlockEntry _ e =
  fail ("unexpected module block entry: " ++ show e)

parseFunProto :: Record -> PartialModule -> Parse PartialModule
parseFunProto r pm = label "FUNCTION" $ do
  ix   <- nextValueId
  (name, offset) <- oldOrStrtabName ix r
  let field i = parseField r (i + offset)
  funTy   <- getType =<< field 0 numeric
  let ty = case funTy of
             PtrTo _  -> funTy
             _        -> PtrTo funTy

  isProto <-             field 2 numeric

  link    <-             field 3 linkage

  section <-
    if length (recordFields r) >= 6
       then do sid <- field 6 numeric
               if sid == 0
                  then return Nothing
                  else do let sid' = sid - 1
                          when (sid' >= Seq.length (partialSections pm))
                              (fail "invalid section name index")
                          return (Just (Seq.index (partialSections pm) sid'))

       else return Nothing

  -- push the function type
  _    <- pushValue (Typed ty (ValSymbol name))
  let lkMb t x
       | Seq.length t > x = Just (Seq.index t x)
       | otherwise        = Nothing
  comdat <- if length (recordFields r) >= 12
               then do comdatID <- field 12 numeric
                       pure (fst <$> partialComdat pm `lkMb` comdatID)
               else pure Nothing
  let proto = FunProto
        { protoType  = ty
        , protoLinkage =
          do -- we emit a Nothing here to maintain output compatibility with
             -- llvm-dis when linkage is External
             guard (link /= External)
             return link
        , protoGC    = Nothing
        , protoSym   = name
        , protoIndex = ix
        , protoSect  = section
        , protoComdat = comdat
        }

  if isProto == (0 :: Int)
     then pushFunProto proto >> return pm
     else return pm { partialDeclares = partialDeclares pm Seq.|> proto }


addGlobalAttachments :: PGlobalAttachments -> (PartialModule -> PartialModule)
addGlobalAttachments gs0 pm = pm { partialGlobals = go (partialGlobals pm) gs0 }
  where

  go gs atts | Map.null atts = gs

  go gs atts =
    case Seq.viewl gs of
      Seq.EmptyL -> Seq.empty

      g Seq.:< gs' ->
        let (mb,atts') = Map.updateLookupWithKey (\_ _ -> Nothing) (pgSym g) atts
         in case mb of
              Just md -> g { pgMd = md } Seq.<| go gs' atts'
              Nothing -> g               Seq.<| go gs' atts'
