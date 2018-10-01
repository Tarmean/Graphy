{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language LambdaCase #-}

module QuickSI where
import qualified Data.Graph.Inductive as G

import Control.Monad.State
import Types
import Control.Lens
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import Data.Coerce
import System.Random.Shuffle
import System.Random


runQuickSI
    :: (G.DynGraph g)
    => StdGen -> (n -> l -> Bool) -> g n e -> [NodeMatcher l] -> [M.Map PatternNode GraphNode]
runQuickSI gen test g patt = evalStateT (runAlg algorithm) env
  where
    env = QuickSIEnv
        { _quickSIEnvGraph = g
        , _quickSIEnvMappings  = M.empty
        , _quickSIEnvMatchers  = patt
        , _quickSIEnvRng = gen
        , _quickSIEnvTester = test
        }

algorithm :: (Graph g, G.DynGraph (BaseGraph g)) => Alg g l (M.Map PatternNode GraphNode)
algorithm = do
   isDone <- checkDone
   if isDone
   then use mappings
   else do
       (p, g) <- candidates
       modifying mappings (M.insert p g)
       algorithm

candidates :: Graph g => Alg g l (PatternNode, GraphNode)
candidates = do
    matcher <- popMatcher
    node <- availableSuccessors matcher

    nodeUsed <- isUsed node
    guard (not nodeUsed)

    curLabel <- lookupLabel node
    comparator <- use tester
    guard (curLabel `comparator` matcherLabel matcher)

    guardAll (checkConstraint node) (constraints matcher)

    return (source matcher, node)
  where guardAll predicate ls = guard . and =<< traverse predicate ls


sample :: [a] -> Alg g l a
sample ls = do
     gen <- use rng
     let (gen', gen'') = split gen
         ls' = shuffle' ls (length ls) gen''
     rng .= gen'
     liftLs ls'
     
checkConstraint :: Graph g => GraphNode -> Constraint -> Alg g l Bool
checkConstraint node constraint = do
    g <- use graph
    case constraint of
        (Degree n) -> return $ G.outdeg g (unGNode node) >= n
        HasEdge other -> return $ G.hasEdge g (unGNode node, other)

availableSuccessors :: Graph g => NodeMatcher l -> Alg g l GraphNode
availableSuccessors matcher = do
    case parent matcher of
        Nothing -> allNodes
        Just curParent -> do
          previous <- lookupMapping curParent
          neighbors previous

checkDone :: Alg g l Bool
checkDone = uses matchers null
    
-- TODO: Sort by outgoing edges to limit branching factor?
allNodes :: Graph g => Alg g l GraphNode
allNodes = sample =<< uses graph  (coerce . G.nodes)

neighbors :: (Graph g) => GraphNode -> Alg g l GraphNode
neighbors node = do 
    curGraph <- use graph
    sample $ (\(_, o, _) -> GraphNode o) <$> G.out curGraph (unGNode node)


lookupLabel :: G.Graph g => GraphNode -> Alg (g n e) l n
lookupLabel node = do
    curGraph <- use graph
    case G.lab curGraph (unGNode node) of
      Just d -> return (d)
      Nothing -> error "Graph node not found"

        
popMatcher :: Alg g l (NodeMatcher l)
popMatcher = do
    cur <- uses matchers head
    modifying matchers tail
    return cur

lookupMapping :: PatternNode -> Alg g l GraphNode
lookupMapping node = do
    curMappings <- use mappings
    return (curMappings M.! node)

isUsed :: GraphNode -> Alg g l Bool
isUsed node =  uses mappings (any (==node) . M.elems)
