{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language TypeApplications, ScopedTypeVariables #-}
module SpanningTree where
import qualified Data.Graph.Inductive as G
import qualified Data.Foldable as F
import qualified Data.Ord as O
import qualified Data.Set as S
import Data.Monoid ((<>))
import Control.Lens
import qualified Control.Monad.Logic as L
import Control.Monad.Logic (lift, guard)

import Types

mst :: MST g ()
mst = do
    g <- use graph
    let nodeCount = length (G.nodes g)
    getFirstEdge >>= addFirstEdge
    let loop = do
        processedCount <- use (verts . to length)
        if processedCount /= nodeCount
        then getSpanningEdge >>= addEdge >> loop
        else return ()
    loop

getFirstEdge :: MST g (LabeledEdge g)
getFirstEdge  = do
    curGraph <- use graph
    let allEdges = G.labEdges (curGraph)
    return (selectFirstEdge curGraph allEdges)

selectFirstEdge :: (WeightedGraph g) => g -> [LabeledEdge g] -> LabeledEdge g
selectFirstEdge g = F.minimumBy (O.comparing edgeWeight <> O.comparing combinedDeg)
 where
   edgeWeight = getWeight . G.edgeLabel
   combinedDeg (fromN, toN, _) = deg fromN + deg toN
   deg = G.outdeg g

adjacentNodes :: Graphy g => g -> S.Set PatternNode -> [LabeledEdge g]
adjacentNodes g seen = filter isNeighbor $ G.labEdges $ g
  where
    isNeighbor (fromN, toN, _) = fromN `S.member` seen && toN `S.notMember` seen

getSpanningEdge :: MST g (LabeledEdge g)
getSpanningEdge = do 
    curGraph <- use graph 
    seen <- use verts
    let candidateEdges = adjacentNodes curGraph seen
    return (selectSpanningEdge curGraph seen candidateEdges)

selectSpanningEdge :: (WeightedGraph g) => g -> S.Set PatternNode -> [LabeledEdge g] -> LabeledEdge g 
selectSpanningEdge g seen
    = F.minimumBy 
        (  O.comparing edgeWeight
        <> O.comparing induced
        <> O.comparing outdeg
        )
  where
    edgeWeight = getWeight . G.edgeLabel
    outdeg (_, toN, _) = G.outdeg g toN

    induced (_,toN,_) = O.Down $ G.order (G.subgraph nodes g)
      where nodes = S.toList (S.insert toN seen)
      
    
dropLabel :: G.LEdge l -> G.Edge
dropLabel (fromN, toN, _label) = (fromN, toN)

makeMatcher :: LabeledEdge g -> MST g (NodeMatcher (GetLabel (EdgeLabel g)))
makeMatcher (fromN, toN, l) = do
    g <- use graph
    missedEdges <- getMissedEdges toN
    let edgeConstraints = HasEdge <$> missedEdges
        deg = G.deg g toN
        addDegConstraint = if deg >= 3 then (Degree deg:) else id
    return NodeMatcher
           { parent = Just fromN 
           , label = getLabel l
           , constraints = addDegConstraint edgeConstraints
           }

getMissedEdges :: WeightedGraph g => G.Node -> MstMonad g [Node]
getMissedEdges node = L.observeAllT $ do
    g <- lift (use graph)
    seen <- lift (use verts)
    (fromN, toN, _) <- toLogicT (G.out g node)
    guard (toN `S.member` seen)
    lift $ modifying graph (G.delEdge  (fromN, toN))
    return toN
            
    
addFirstEdge :: LabeledEdge g -> MST g ()
addFirstEdge e@(fromN, _, _) = do
    addEdge e
    modifying verts (S.insert fromN)

addEdge  :: LabeledEdge g -> MST g ()
addEdge e@(fromN, toN, _label) = do
    modifying verts (S.insert toN)
    modifying edges (S.insert (fromN, toN))
    matcher <- makeMatcher e
    modifying matchers (++[matcher])
    return ()

