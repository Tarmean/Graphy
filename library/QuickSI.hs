{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
module QuickSI where
import qualified Data.Graph.Inductive as G

import Types

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Generic as V
import qualified Control.Monad.Logic as L
import Control.Monad.ST

import Control.Monad
import Control.Monad.Reader

liftST :: ST s a -> Alg s g a
liftST = Alg . lift . lift

liftLs :: [a] -> Alg s g a
liftLs = Alg . lift . toLogicT
  where toLogicT ls = L.LogicT $ \cons zero -> foldr cons zero ls

readMVec :: (VU.Unbox a) => VU.MVector s a -> Int -> Alg s g a
readMVec vec i = liftST (vec `VM.read` i)

getParentMapping :: Alg s g (Maybe GraphNode)
getParentMapping = do
    matcher <- getMatcher
    traverse lookupMapping $ parent matcher
        
getMatcher :: Alg s g (NodeMatcher (NodeLabel g))
getMatcher = do
    depth <- asks envDepth
    matchers <- asks envMatchers
    return (matchers V.! depth)

lookupMapping :: PatternNode -> Alg s g GraphNode
lookupMapping node = do
    mappings <- asks envMappings
    readMVec mappings node

isUsed :: PatternNode -> Alg s g Bool
isUsed node = asks envUsed >>= (`readMVec` node)

availableSuccessors :: ALG s g GraphNode
availableSuccessors = do
    mparent <- getParentMapping
    case mparent of
        Nothing -> allNodes
        Just previous -> neighbors previous

-- todo clean this up
withNode :: Node -> Alg s g r -> Alg s g r
withNode node cont = do
    usedVec <- asks envUsed
    mappingsVec <- asks envMappings
    depth <- asks envDepth

    liftST (VM.write usedVec node True)
    liftST (VM.write mappingsVec depth node)

    r <- local (\r -> r { envDepth = envDepth r + 1 }) cont

    liftST (VM.write usedVec node False)

    return r

algorithm :: (Eq (NodeLabel g)) => ALG s g (VU.Vector GraphNode)
algorithm = do
   isDone <- checkDone
   if isDone
   then do
       mappings <- asks envMappings
       liftST (VU.freeze mappings)
   else do
       node <- candidates
       withNode node algorithm

checkDone :: Alg s g Bool
checkDone = do
    depth <- asks envDepth
    mappings <- asks envMappings
    let maxDepth = VM.length mappings
    return (depth > maxDepth)
    

candidates :: (Eq (NodeLabel g)) => ALG s g GraphNode
candidates = do
    matcher <- getMatcher
    node <- availableSuccessors 

    used <- isUsed node
    guard (not used)

    curLabel <- lookupLabel node
    guard (curLabel == label matcher)

    graph <- asks envGraph
    let
      checkConstraint (Degree n) = G.deg graph node >= n
      checkConstraint (HasEdge other) = G.hasEdge graph (node, other)

    guard $ all checkConstraint (constraints matcher)

    return node

lookupLabel :: GraphNode -> ALG s g (NodeLabel g)
lookupLabel node = do
    graph <- asks envGraph
    Just lbl <- pure $ G.lab graph node
    return lbl

allNodes :: ALG s g GraphNode
allNodes = liftLs =<< asks (G.nodes . envGraph)
neighbors :: (Graphy g) => GraphNode -> Alg s g GraphNode
neighbors node = do 
    graph <- asks envGraph
    liftLs $ G.neighbors graph node
