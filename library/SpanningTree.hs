{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
module SpanningTree where
import qualified Data.Graph.Inductive as G
import qualified Data.Foldable as F
import qualified Data.Ord as O
import qualified Data.Set as S
import Data.Monoid ((<>))
import Control.Lens
import qualified Control.Monad.Logic as L
import Control.Monad.Logic (lift, guard)
import Control.Monad (when)
import Data.Coerce
import Data.Maybe (listToMaybe)

import Types

type AnnotatedEdge g = G.LEdge (EdgeData g)

runMST :: (G.DynGraph g) => (Node -> Node -> Double) -> (g n e) -> [NodeMatcher n]
runMST edgeWeight baseGraph = view matchers $ snd $ runMstMonad mst (addWeights edgeWeight baseGraph)

mst :: (IsWeighted (EdgeData g), DynGraph g) => MstMonad g ()
mst = do
    g <- use graph
    let nodeCount = length (G.nodes g)
    firstNode <- getFirstNode 
    maybe (return ()) addFirstNode firstNode

    let loop = do
                processedCount <- use (verts . to length)
                when (processedCount /= nodeCount)
                   $ getSpanningEdge >>= addEdge >> loop
    loop

getFirstNode :: (IsWeighted (EdgeData g), Graph g) => MstMonad g (Maybe Node)
getFirstNode  = do
    curGraph <- use graph
    let allEdges = G.labEdges curGraph
    if null allEdges
    then return $ listToMaybe (G.nodes curGraph)
    else return $ Just $ getNode $ selectFirstEdge curGraph allEdges
  where
    selectFirstEdge g = F.minimumBy (O.comparing edgeWeight <> O.comparing combinedDeg)
      where combinedDeg (fromN, toN, _) = G.outdeg g fromN + G.outdeg g toN
    edgeWeight = getWeight . G.edgeLabel
    getNode (a, _, _) = a

getSpanningEdge :: (IsWeighted (EdgeData g), DynGraph g) => MstMonad g (AnnotatedEdge g)
getSpanningEdge = do 
    curGraph <- use graph 
    seen <- use verts
    let candidateEdges = adjacentNodes curGraph seen
    if null candidateEdges
    then error "Error: Pattern has to be connected for QuickSI to work"
    else return (selectSpanningEdge curGraph seen candidateEdges)

selectSpanningEdge :: (IsWeighted (EdgeData g), DynGraph g) => g -> S.Set Node -> [AnnotatedEdge g] -> AnnotatedEdge g 
selectSpanningEdge g seen
    = F.minimumBy 
        (  O.comparing edgeWeight
        <> O.comparing induced
        <> O.comparing outdeg
        )
  where
    edgeWeight = getWeight . G.edgeLabel
    outdeg (_, toN, _) = G.outdeg g toN

    induced (_,toN,_) = O.Down $ edgeCount (G.subgraph matchedNodes g)
      where matchedNodes = S.toList (S.insert toN seen)
            edgeCount = length . G.edges

makeFirstMatcher :: (Graph g) => Node -> MstMonad g (Matcher g)
makeFirstMatcher fromN = do
    g <- use graph
    l <- lookupLabel fromN
    return NodeMatcher
        { parent = Nothing
        , matcherLabel = l
        , constraints = addDegConstraint g fromN []
        , source = PatternNode fromN
        }
makeMatcher :: (DynGraph g) => AnnotatedEdge g -> MstMonad g (Matcher g)
makeMatcher (fromN, toN, _) = do
    g <- use graph
    missedEdges <- getMissedEdges toN
    let edgeConstraints = map HasEdge $ coerce $ filter (/= fromN) missedEdges
    l <- lookupLabel toN
    return NodeMatcher
           { parent = Just (PatternNode fromN)
           , matcherLabel = l
           , constraints = addDegConstraint g toN edgeConstraints
           , source = PatternNode toN
           }
addFirstNode :: (DynGraph g) => Node -> MstMonad g ()
addFirstNode fromN = do
    initialMatcher <- makeFirstMatcher fromN
    modifying matchers (initialMatcher:)
    modifying verts (S.insert fromN)

addEdge  :: (DynGraph g) => AnnotatedEdge g -> MstMonad g ()
addEdge e@(fromN, toN, _label) = do
    modifying verts (S.insert toN)
    modifying edges (S.insert (fromN, toN))
    matcher <- makeMatcher e
    modifying matchers (++[matcher])
    return ()

lookupLabel :: (Graph g) => Node -> MstMonad g (NodeData g)
lookupLabel n = do
    g <- use graph
    Just l <- return (G.lab g n)
    return l
addDegConstraint :: Graph g => g -> Node -> ([Constraint] -> [Constraint])
addDegConstraint g toN = if deg >= 3 then (Degree deg:) else id
  where deg = G.outdeg g toN

getMissedEdges :: DynGraph g => G.Node -> MstMonad g [Node]
getMissedEdges node = L.observeAllT $ do
    g <- lift (use graph)
    seen <- lift (use verts)
    (fromN, toN, _) <- toLogicT (G.out g node)
    guard (toN `S.member` seen)
    lift $ modifying graph (G.delEdge  (fromN, toN))
    return toN
            
adjacentNodes :: Graph g => g -> S.Set Node -> [AnnotatedEdge g]
adjacentNodes g seen = filter isNeighbor $ G.labEdges g
  where
    isNeighbor (fromN, toN, _) = fromN `S.member` seen && toN `S.notMember` seen


addWeights :: G.DynGraph g => (Node -> Node -> Double) ->  g n e -> g n (Weighted e)
addWeights edgeWeight = G.gmap step
  where
    step (fromEdges, node, lbl, toEdges) =
        ( map (wInEdge node) fromEdges
        , node
        , lbl
        , map (wOutEdge node) toEdges
        )
    wInEdge node (lbl, other) = (Weighted (edgeWeight other node) lbl, other)
    wOutEdge node (lbl, other) = (Weighted (edgeWeight node other) lbl, other)
