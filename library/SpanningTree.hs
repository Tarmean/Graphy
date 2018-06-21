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

runMST :: (Graph g, IsUnweighted (NodeData g)) => (Node -> Node -> Double) -> (Node -> Double) -> g -> [Matcher g]
runMST edgeWeight nodeWeight baseGraph = view matchers $ runMstMonad mst (addWeights edgeWeight nodeWeight baseGraph)

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

getFirstEdge :: MST g (AnnotatedEdge g)
getFirstEdge  = do
    curGraph <- use graph
    let allEdges = G.labEdges (curGraph)
    return (selectFirstEdge curGraph allEdges)

selectFirstEdge :: (WeightedGraph g) => g -> [AnnotatedEdge g] -> AnnotatedEdge g
selectFirstEdge g = F.minimumBy (O.comparing edgeWeight <> O.comparing combinedDeg)
 where
   edgeWeight = getWeight . G.edgeLabel
   combinedDeg (fromN, toN, _) = deg fromN + deg toN
   deg = G.outdeg g

getSpanningEdge :: MST g (AnnotatedEdge g)
getSpanningEdge = do 
    curGraph <- use graph 
    seen <- use verts
    let candidateEdges = adjacentNodes curGraph seen
    return (selectSpanningEdge curGraph seen candidateEdges)

selectSpanningEdge :: (WeightedGraph g) => g -> S.Set PatternNode -> [AnnotatedEdge g] -> AnnotatedEdge g 
selectSpanningEdge g seen
    = F.minimumBy 
        (  O.comparing edgeWeight
        <> O.comparing induced
        <> O.comparing outdeg
        )
  where
    edgeWeight = getWeight . G.edgeLabel
    outdeg (_, toN, _) = G.outdeg g toN

    induced (_,toN,_) = O.Down $ edgeCount (G.subgraph nodes g)
      where nodes = S.toList (S.insert toN seen)
            edgeCount = length . G.edges

adjacentNodes :: Graph g => g -> S.Set PatternNode -> [AnnotatedEdge g]
adjacentNodes g seen = filter isNeighbor $ G.labEdges g
  where
    isNeighbor (fromN, toN, _) = fromN `S.member` seen && toN `S.notMember` seen

makeFirstMatcher :: AnnotatedEdge g -> MST g (Matcher g)
makeFirstMatcher (fromN, _, _) = do
    g <- use graph
    l <- lookupLabel fromN
    return NodeMatcher
        { parent = Nothing
        , matcherLabel = l
        , constraints = addDegConstraint g fromN []
        }
makeMatcher :: AnnotatedEdge g -> MST g (Matcher g)
makeMatcher (fromN, toN, _) = do
    g <- use graph
    missedEdges <- getMissedEdges toN
    let edgeConstraints = HasEdge <$> missedEdges
    l <- lookupLabel toN
    return NodeMatcher
           { parent = Just fromN 
           , matcherLabel = l
           , constraints = addDegConstraint g toN edgeConstraints
           }
lookupLabel :: Node -> MST g (GetLabel (NodeData g))
lookupLabel n = do
    g <- use graph
    Just l <- return (G.lab g n)
    return (l ^. label)
addDegConstraint :: Graph g => g -> Node -> ([Constraint] -> [Constraint])
addDegConstraint g toN = if deg >= 3 then (Degree deg:) else id
  where deg = G.deg g toN

getMissedEdges :: WeightedGraph g => G.Node -> MstMonad g [Node]
getMissedEdges node = L.observeAllT $ do
    g <- lift (use graph)
    seen <- lift (use verts)
    (fromN, toN, _) <- toLogicT (G.out g node)
    guard (toN `S.member` seen)
    lift $ modifying graph (G.delEdge  (fromN, toN))
    return toN
            
    
addFirstEdge :: AnnotatedEdge g -> MST g ()
addFirstEdge e@(fromN, _, _) = do
    addEdge e
    modifying verts (S.insert fromN)

addEdge  :: AnnotatedEdge g -> MST g ()
addEdge e@(fromN, toN, _label) = do
    modifying verts (S.insert toN)
    modifying edges (S.insert (fromN, toN))
    matcher <- makeMatcher e
    modifying matchers (++[matcher])
    return ()

addWeights :: Graph g => (Node -> Node -> Double) -> (Node -> Double) -> g -> WithWeights g
addWeights edgeWeight nodeWeight = G.gmap step
  where
    step (inEdges, node, lbl, outEdges) =
        ( map (wInEdge node) inEdges
        , node
        , wSelf node lbl
        , map (wOutEdge node) outEdges
        )
    wInEdge node (lbl, other) = (Weighted (edgeWeight other node) lbl, other)
    wOutEdge node (lbl, other) = (Weighted (edgeWeight node other) lbl, other)
    wSelf node lbl = Weighted (nodeWeight node) lbl
