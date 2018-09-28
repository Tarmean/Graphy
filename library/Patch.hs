{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
module Patch where
import Types
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M



applyPatch :: (Graph g) => M.Map Node Node -> GraphPatch (NodeData g) (EdgeData g) -> g -> g
applyPatch m (ReplaceEdge bFrom bTo lbls) = addNew . removeOld
  where
    from = m M.! bFrom
    to = m M.! bTo

    lEdges = map (\l -> (from, to, l)) lbls
    removeOld = G.delEdge (from, to)
    addNew = G.insEdges lEdges
    
    
applyPatch m (DeleteNode node) = G.delNode (m M.! node)
-- applyPatch m (AddNode node) g = DeleteNode (m M.! node)
