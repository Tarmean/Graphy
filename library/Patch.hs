{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
{-# Language TupleSections #-}
{-# Language ScopedTypeVariables #-}
module Patch where
import Types
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Map as M
import Control.Lens hiding (from, to)
import UnsafeNextGraphId
import Data.Coerce
import Control.Monad.State (execState)

runPatch :: (g ~ Gr.Gr v e) => PatchAlg g a -> g -> g -> M.Map PatternNode GraphNode -> g
runPatch patch g p m = view graph $ execState (unPatch patch) (RewriteEnv g m p (nextId g))

class Patch m where
    type EdgeLabel m
    type NodeLabel m

    -- Given these edges in the graph
    --
    -- | from | to | label |
    -- |------+----+-------|
    -- | 1    | 2  | a     |
    -- | 1    | 2  | a     |
    -- | 1    | 2  | b     |
    --
    -- and a pattern
    --
    -- 1 -> 2 (a)
    --
    -- to be replaced by
    --
    -- 1 -> 2 (b)
    --
    -- should we get
    --
    -- | from | to | label |
    -- |------+----+-------|
    -- | 1    | 2  | a     |
    -- | 1    | 2  | b     |
    -- | 1    | 2  | b     |
    --
    -- or
    --
    -- | from | to | label |
    -- |------+----+-------|
    -- | 1    | 2  | b     |
    -- | 1    | 2  | b     |
    --
    -- or
    --
    -- | from | to | label |
    -- |------+----+-------|
    -- | 1    | 2  | b     |
    --
    -- ?
    --
    -- replaceEdges gives #2, replaceNode gives #3


    deleteNode :: PatternNode -> m ()
    replaceEdges :: PatternNode -> PatternNode -> [EdgeLabel m] -> m ()
    replaceNode
        :: [(EdgeLabel m, PatternNode)]
        -> PatternNode
        -> NodeLabel m 
        -> [(EdgeLabel m, PatternNode)]
        -> m ()
    labelOf :: PatternNode -> m (NodeLabel m)

instance (Eq e) => Patch (PatchAlg (Gr.Gr v e)) where
    type EdgeLabel (PatchAlg (Gr.Gr v e)) = e
    type NodeLabel (PatchAlg (Gr.Gr v e)) = v
    deleteNode pNode = do
       node <- translate pNode
       modifying graph (G.delNode $ unGNode node)
    replaceEdges pFrom pTo labs = do
        obsoleteEdges <- getOldEdges pFrom pTo
        modifying graph (removeEdges obsoleteEdges)
        from <- translate pFrom
        to <- translate pTo
        let newEdges = map (from, to,) labs
        modifying graph (G.insEdges $ coerce newEdges)
      where
        removeEdges ls g = foldr G.delAllLEdge g $ isList $ coerce $ ls
        isList :: [a] -> [a]
        isList = id
    replaceNode inE pNode nlbl outE = do
        node <- translate pNode
        modifying mappings (M.insert pNode node)
        toEdges <- traverseOf (each . _2) translate inE
        fromEdges <- traverseOf (each . _2) translate outE
        modifying graph (coerce (toEdges, node, nlbl, fromEdges) G.&)
    labelOf node = do
        m <- uses pattern (\g -> G.lab g $ unPNode node)
        case m of
            Just n -> return n
            Nothing -> error "Node not in pattern"
        
getOldEdges :: PatternNode -> PatternNode -> PatchAlg (Gr.Gr v e) [(GraphNode, GraphNode, e)]
getOldEdges pFrom pTo = do
    p <- use pattern
    let successors = G.lsuc p (unPNode pFrom)
        correctEdges = filter ((==unPNode pTo) . fst) successors
    from <- translate pFrom
    to <- translate pTo
    let labeledEdges = map (\(_, l) -> (from, to, l))  correctEdges
    return labeledEdges

translate :: PatternNode -> PatchAlg (Gr.Gr v e) GraphNode
translate node = do
    m <- use mappings
    case M.lookup node m of
        Just v -> return v
        Nothing -> do
           new <- use maxKey
           modifying maxKey (+1)
           modifying mappings (M.insert node new)
           return new
