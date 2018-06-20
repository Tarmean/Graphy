{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
module SpanningTree where
import qualified Data.Graph.Inductive as G
import qualified Data.Foldable as F
import qualified Data.Ord as O
import qualified Data.Set as S
import Data.Monoid ((<>))
import Control.Monad.State

import Types

firstEdge :: MST g (LabeledEdge g)
firstEdge  = do
    graph <- gets mstGraph
    let edges = G.labEdges (graph)
    return (selectFirstEdge graph edges)

selectFirstEdge :: (WeightedGraph g) => g -> [LabeledEdge g] -> LabeledEdge g
selectFirstEdge g = F.minimumBy (O.comparing edgeWeight <> O.comparing combinedDeg)
 where
   edgeWeight = getWeight . G.edgeLabel
   combinedDeg (from, to, _) = deg from + deg to
   deg = G.outdeg (g)

adjacentNodes :: Graphy g => g -> S.Set PatternNode -> [LabeledEdge g]
adjacentNodes g seen = filter isNeighbor $ G.labEdges $ g
  where
    isNeighbor (from, to, _) = from `S.member` seen && to `S.notMember` seen

spanningEdge :: MST g [LabeledEdge g]
spanningEdge = adjacentNodes <$> (gets mstGraph) <*> (gets mstVerts)

selectSpanningEdge :: (WeightedGraph g) => g -> S.Set PatternNode -> [LabeledEdge g] -> LabeledEdge g 
selectSpanningEdge g seen
    = F.minimumBy 
        (  O.comparing edgeWeight
        <> O.comparing induced
        <> O.comparing outdeg
        )
  where
    edgeWeight = getWeight . G.edgeLabel
    outdeg (_, to, _) = G.outdeg (g) to

    induced (_,to,_) = G.order $ G.subgraph nodes g
      where nodes = S.toList (S.insert to seen)
      
    
dropLabel :: G.LEdge l -> G.Edge
dropLabel (from, to, _label) = (from, to)

-- recordFirstEdge :: LabeledEdge g -> MST g ()
-- recordFirstEdge edge = do
--     -- modify' (S.insert $ dropLabel edge)
--     return ()

-- mst :: forall g. (G.Graph g) => g Double Double -> _
-- mst g0 = loop g0 (S.singleton (root,first)) (S.fromList [root, first])
--   where

--     edges = G.labEdges g0
--
--     addSeq seq (parent, child) graph = _
--       where
--         matcher = NodeMatcher (Just parent) (G.lab graph child) constraints

--     minimumEdgeWeight = minimum $ thrd <$> edges
--     smallestEdges = filter ((==minimumEdgeWeight).thrd) edges
--     (root,first,l) = selectFirstEdge smallestEdges g0

--     selectFirstEdge :: [G.LEdge Double] -> g Double Double -> G.LEdge Double
--     selectFirstEdge = _
--     selectSpanningEdge :: [G.Edge] -> g Double Double -> S.Set PatternNode -> G.Edge
--     selectSpanningEdge candidates graph used = _

--         constraints
          
--         otherEdges = [(l,r)| (l,r) <- G.edges graph, valid l r || valid r l]
--         valid l r = l == child && r `S.elem` verts

--     loop :: g Double Double -> S.Set (G.Edge) -> S.Set PatternNode -> _ -> _
--     loop g edges verts seq
--       | null edge_candidates = seq
--       | otherwise = loop g' edges' verts' seq'
--       where
--         edge_candidates = []
--         edge@(parent, child) = selectSpanningEdge edge_candidates g verts

--         g' = G.delNode child g
--         edges' = S.insert edge edges
--         verts' = S.insert child verts
--         seq' = addSeq seq edge g' verts'


