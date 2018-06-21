{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
module QuickSI where
import qualified Data.Graph.Inductive as G

import Types

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Generic as V
import Control.Lens

import Control.Monad
import Control.Monad.Reader

algorithm :: (Eq (GetLabel (NodeData g))) => ALG s g (VU.Vector GraphNode)
algorithm = do
   isDone <- checkDone
   if isDone
   then do
       choices <- view mappings
       liftST (VU.freeze choices)
   else do
       node <- candidates
       withNode node algorithm

candidates :: (Eq (GetLabel (NodeData g))) => ALG s g GraphNode
candidates = do
    matcher <- getMatcher
    node <- availableSuccessors 

    nodeUsed <- isUsed node
    guard (not nodeUsed)

    curLabel <- lookupLabel node
    guard (curLabel == matcherLabel matcher)

    g <- view graph
    let
      checkConstraint (Degree n) = G.deg g node >= n
      checkConstraint (HasEdge other) = G.hasEdge g (node, other)

    guard $ all checkConstraint (constraints matcher)

    return node

availableSuccessors :: ALG s g GraphNode
availableSuccessors = do
    mparent <- getParentMapping
    case mparent of
        Nothing -> allNodes
        Just previous -> neighbors previous

checkDone :: Alg s g Bool
checkDone = do
    curDepth <- view depth
    curMappings <- view mappings
    let maxDepth = VM.length curMappings
    return (curDepth > maxDepth)
    
allNodes :: ALG s g GraphNode
allNodes = liftLs =<< view (graph . to G.nodes)

neighbors :: (Graph g) => GraphNode -> Alg s g GraphNode
neighbors node = do 
    curGraph <- view graph
    liftLs $ G.neighbors curGraph node


lookupLabel :: GraphNode -> ALG s g (GetLabel (NodeData g))
lookupLabel node = do
    curGraph <- view graph
    Just lbl <- pure $ G.lab curGraph node
    return (lbl ^. label)

readMVec :: (VU.Unbox a) => VU.MVector s a -> Int -> Alg s g a
readMVec vec i = liftST (vec `VM.read` i)

getParentMapping :: Alg s g (Maybe GraphNode)
getParentMapping = do
    matcher <- getMatcher
    traverse lookupMapping $ parent matcher
        
getMatcher :: Alg s g (Matcher g)
getMatcher = do
    curDepth <- view depth
    curMatchers <- view matchers
    return (curMatchers V.! curDepth)

lookupMapping :: PatternNode -> Alg s g GraphNode
lookupMapping node = do
    curMappings <- view mappings
    readMVec curMappings node

isUsed :: PatternNode -> Alg s g Bool
isUsed node = view used >>= (`readMVec` node)

withNode :: Node -> Alg s g r -> Alg s g r
withNode node cont = do
    usedVec <- view used
    mappingsVec <- view mappings
    curDepth <- view depth

    liftST (VM.write usedVec node True)
    liftST (VM.write mappingsVec curDepth node)

    r <- local (depth +~ 1) cont

    liftST (VM.write usedVec node False)

    return r
