{- |
Module           : $Header$
Description      : LLVM control flow graphs and related utilities
Stability        : provisional
Point-of-contact : jstanley
-}
{-# LANGUAGE BangPatterns                #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE ViewPatterns                #-}
{-# LANGUAGE CPP                         #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.LLVM.CFG
  ( CFG(..)
  , BB
  , BBId
  , blockId
  , blockName
  , buildCFG
  , dummyExitName
  )
where

#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative
#endif

import           Control.Arrow

import           Data.Functor.Identity (runIdentity)
import qualified Data.Graph.Inductive.Query.Dominators as Dom
import qualified Data.Graph.Inductive                  as G
import qualified Data.Map                              as M

import           Text.LLVM                             hiding (BB)
import qualified Text.LLVM.Labels                      as L

-- import Debug.Trace

newtype BBId = BBId { unBBId :: G.Node } deriving (Eq)

type BB = BasicBlock' (BBId, BlockLabel)

-- | The control-flow graph for LLVM functions
data CFG = CFG
  { cfgGraph :: G.Gr BB ()
  -- | The @BBId@ of the entry node in the control-flow graph
  , entryId :: BBId
  -- | The @BBId@ of the exit node from the control-flow graph
  , exitId :: BBId
  -- | All basic blocks in the CFG
  , allBBs :: [BB]
  -- | Obtain a basic block from a @BBId@ (runtime error if it DNE)
  , bbById :: BBId -> BB
  -- | Obtain the @BBId@ of a block from its name (runtime error if it DNE)
  , asId :: BlockLabel -> BBId
  -- | Obtain the name of a block from a @BBId@ (runtime error if it DNE)
  , asName :: BBId -> BlockLabel
  -- | Obtain all predecessor basic blocks from a @BBId@
  , bbPreds :: BBId -> [BBId]
  -- | Obtain all successor basic blocks from a @BBId@
  , bbSuccs :: BBId -> [BBId]
  -- | @dom x y@ yields True iff x dominates y in the CFG (i.e., all paths from
  -- the entry node to y must pass through x)
  , dom :: BBId -> BBId -> Bool
  -- | @idom x@ yields the unique immediate dominator of x in the CFG
  -- (intuitively, the "nearest" dominator of x; formally, y immediately
  -- dominates x iff y dominates x and there is no intervening block z such that
  -- y dominates z and z dominates x).  The entry node has no immediate
  -- dominator.
  , idom :: BBId -> Maybe BBId
    -- | @pdom x y@ yields True iff x postdominates y in the CFG (i.e., all
    -- paths in the CFG from y to the exit node pass through x)
  , pdom :: BBId -> BBId -> Bool
    -- | @ipdom x@ yields the unique immediate postdominator of x in the CFG
    -- (intuitively, the "nearest" postdominator; formally, y immediately
    -- postdominates x iff y postdominates x and there is no intervening block z
    -- such that y postdominates z and z postdominates x).  The exit node has no
    -- immediate postdominator.
  , ipdom :: BBId -> Maybe BBId
    -- | @pdom@ yields post-dominator analysis for the entire CFG; the resulting
    -- list associates each node with a list of its postdominators.  The
    -- postdominator list is sorted in order of ascending immediacy; i.e., the
    -- last element of the list associated with a node @n@ is @n@'s immediate
    -- dominator, the penultimate element of the list is the immediate
    -- postdominator of @n@'s immediate postdominator, and so forth.  NB: note
    -- the postdominator lists do not explicitly reflect that a node
    -- postdominates itself.
  , pdoms :: [(BBId, [BBId])]
  }

dummyExitName :: String
dummyExitName = "_dummy_exit"

-- | Builds the control-flow graph of a function.  Assumes that the entry node
-- is the first basic block in the list. Note that when multiple exit nodes are
-- present in the list, they will all end up connected to a single, unique
-- "dummy" exit node.  Note, also, that the CFG basic blocks are of type
-- @BasicBlock' (BBId, Ident)@; that is, they are all named, which is not the
-- case with the input BBs.  It is expected that clients use these versions of
-- the basic blocks rather than those that are passed in.
buildCFG :: [BasicBlock] -> CFG
buildCFG bs = cfg
  where
    cfg = CFG
          { cfgGraph = gr
          , entryId  = BBId 0
          , exitId   = BBId (either id id exit)
          , allBBs   = getBBs gr (G.nodes gr)

          , bbById   = \(BBId x) ->
                       case bbFromCtx <$> fst (G.match x gr) of
                         Nothing -> error "buildCFG: bbById: invalid BBId"
                         Just bb -> bb

          , asId     = \ident ->
                       case BBId <$> M.lookup ident nodeByName of
                         Nothing   -> error "buildCFG: asId: invalid ident"
                         Just bbid -> bbid

          , asName   = \bbid              -> blockName $ bbById cfg bbid
          , bbPreds  = \(BBId x)          -> BBId <$> G.pre gr x
          , bbSuccs  = \(BBId x)          -> BBId <$> G.suc gr x
          , dom      = \(BBId x) (BBId y) -> lkupDom domInfo x y
          , idom     = \(BBId x)          -> BBId <$> lookup x idomInfo
          , pdom     = \(BBId x) (BBId y) -> lkupDom pdomInfo x y
          , ipdom    = \(BBId x)          -> BBId <$> lookup x ipdomInfo
          , pdoms    = map (BBId *** map BBId . reverse . drop 1) pdomInfo
          }

    -- Dominance and post-dominance relations
    cdom g (BBId root)    = (Dom.dom g root, Dom.iDom g root)
    (domInfo, idomInfo)   = cdom gr          (entryId cfg)
    (pdomInfo, ipdomInfo) = cdom (G.grev gr) (exitId cfg)
    lkupDom info x y      = maybe False id (elem x <$> lookup y info)

    -- Graph construction
    (exit, gr)    = stitchDummyExit lab (G.mkGraph nodes' edges')
                      where lab n = BasicBlock (Just (BBId n, Named $ Ident dummyExitName))
                                      [Effect Unreachable mempty []]
    nodes'        = map (nodeId &&& id) bs'
    edges'        = concatMap bbOutEdges bs'

    bbOutEdges :: BB -> [G.LEdge ()]
    bbOutEdges bb = edgesTo (brTargets bb)
      where
        srcId    = nodeId bb
        edgesTo  = map (\(BBId tgt,_) -> srcId `to` tgt)

    -- Relabeling and aux data structures; note that unnamed basic blocks get a
    -- generated name here so that clients don't have to deal with extraneous
    -- checks.
    bs' :: [BB]
    (bbIds, bs', _, _) = foldr relabel (M.empty, [], length bs - 1, 0) bs
      where
        relabel (BasicBlock mid stmts) (mp, acc, n, s :: Int) =
--           trace ("relabel: mid = " ++ show mid)
--           $
          let (s', nm) = case mid of
                           Nothing ->  (s + 1, Named $ Ident $ "__anon_" ++ show s)
                           Just nm' -> (s, nm')
              bbid     = (BBId n, nm)
          in
            ( M.insert nm bbid mp
            , BasicBlock (Just bbid) (fixLabels stmts) : acc
            , n - 1
            , s'
            )

    nodeByName = fmap (\(BBId n, _) -> n) bbIds

    fixLabels stmts = runIdentity (mapM (L.relabel f) stmts)
      where
      -- This should be fine, as there shouldn't be references to labels that
      -- aren't defined.
      f _ lab = return (bbIds M.! lab)


--------------------------------------------------------------------------------
-- Utility functions

bbFromCtx :: G.Context BB () -> BB
bbFromCtx (_, _, bb, _) = bb

to :: G.Node -> G.Node -> G.LEdge ()
u `to` v = (u, v, ())

requireLabel :: BB -> (BBId, BlockLabel)
requireLabel bb =
  case bbLabel bb of
    Just lab -> lab
    Nothing  -> error ("requireLabel: basic block without a label\n" ++ show bb)

blockId :: BB -> BBId
blockId = fst . requireLabel

blockName :: BB -> BlockLabel
blockName = snd . requireLabel

nodeId :: BB -> G.Node
nodeId = unBBId . blockId

getBBs :: G.Gr BB () -> [G.Node] -> [BB]
getBBs gr ns =
  case mapM (G.lab gr) ns of
    Just bbs -> bbs
    Nothing  -> error "internal: encountered unlabeled node"

newNode :: G.Graph gr => gr a b -> G.Node
newNode = head . G.newNodes 1

exitNodes :: G.DynGraph gr => gr a b -> [G.Node]
exitNodes gr = map G.node' $ G.gsel (null . G.suc gr . G.node') gr

-- | @stitchDummyExit labf gr@ adds to graph @gr@ a dummy terminal node with a
-- caller-generated label (parameterized by the new exit node id) and connects
-- all other terminal nodes to it, if needed.  The first element of the returned
-- tuple is the id of the exit node (Left: already present, Right: added). The
-- second element of the returned tuple is the (un)modified graph.

stitchDummyExit :: G.DynGraph gr =>
                   (G.Node -> a) -> gr a () -> (Either G.Node G.Node, gr a ())
stitchDummyExit exitLabelF gr = case exitNodes gr of
  []    -> error "internal: input graph contains no exit nodes"
  [n]   -> (Left n, gr)
  exits ->
    let new = newNode gr
        !g0 = G.insNode (new, exitLabelF new) gr
        !g1 = foldr G.insEdge g0 $ map (`to` new) exits
    in (Right new, g1)

instance Show CFG where
  show cfg = unlines [ "Entry : " ++ show (entryId cfg)
                     , "Exit  : " ++ show (exitId cfg)
                     , "Graph : " ++ ( unlines
                                     . map (\s -> replicate 8 ' ' ++ s)
                                     . lines
                                     $ show (cfgGraph cfg)
                                     )
                     ]

instance Show BBId where show (BBId n) = "BB#" ++ show n
