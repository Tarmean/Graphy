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
import qualified Data.Sequence as S
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Coerce


runQuickSI
    :: (Graph g, Eq (GetLabel (NodeData g)))
    => g -> [Matcher g] -> [M.Map PatternNode GraphNode]
runQuickSI g pattern = do
    finalMappings <- evalStateT (runAlg algorithm) env
    return (toMap pattern finalMappings)
  where
    env = QuickSIEnv
        { _quickSIEnvGraph = g
        , _quickSIEnvMappings  = S.empty
        , _quickSIEnvMatchers  = pattern
        }


toMap :: [NodeMatcher l] -> S.Seq Node -> M.Map PatternNode  GraphNode
toMap pattern matchedNodes
    = M.fromList
    $ zip (source <$> pattern) (coerce $ toList matchedNodes)

runStuff :: (Graph g, Eq (GetLabel (NodeData g))) => g -> [Matcher g] -> ALG g r -> [r]
runStuff g pattern m = do
    let env
          = QuickSIEnv { _quickSIEnvGraph = g
          , _quickSIEnvMappings  = S.empty
          , _quickSIEnvMatchers  = pattern
          }
    evalStateT (runAlg m) env


algorithm :: (Eq (GetLabel (NodeData g))) => ALG g (S.Seq Node)
algorithm = do
   isDone <- checkDone
   if isDone
   then use mappings
   else do
       node <- candidates
       modifying mappings (S.|>node)
       algorithm

step :: (Eq (GetLabel (NodeData g))) => ALG g ()
step = candidates >>= \n -> modifying mappings (S.|>n)

candidates :: (Eq (GetLabel (NodeData g))) => ALG g Node
candidates = do
    matcher <- popMatcher
    node <- availableSuccessors  matcher

    nodeUsed <- isUsed node
    guard (not nodeUsed)

    curLabel <- lookupLabel node
    guard (curLabel == matcherLabel matcher)

    guardAll (checkConstraint node) (constraints matcher)

    return node
  where guardAll predicate ls = guard . and =<< traverse predicate ls

checkConstraint :: Node -> Constraint -> ALG g Bool
checkConstraint node constraint = do
    g <- use graph
    case constraint of
        (Degree n) -> return $ G.outdeg g node >= n
        HasEdge other -> return $ G.hasEdge g (node, other)

availableSuccessors :: Matcher g -> ALG g Node
availableSuccessors matcher = do
    case parent matcher of
        Nothing -> allNodes
        Just curParent -> do
          previous <- lookupMapping curParent
          neighbors previous

checkDone :: Alg g Bool
checkDone = uses matchers null
    
-- TODO: Sort by outgoing edges to limit branching factor?
allNodes :: ALG g Node
allNodes = liftLs =<< use (graph . to G.nodes)

neighbors :: (Graph g) => Node -> Alg g Node
neighbors node = do 
    curGraph <- use graph
    liftLs $ (\(_, o, _) -> o) <$> G.out curGraph node


lookupLabel :: Node -> ALG g (GetLabel (NodeData g))
lookupLabel node = do
    curGraph <- use graph
    case G.lab curGraph node of
      Just dat -> return (dat ^. label)
      Nothing -> error "Graph node not found"


        
popMatcher :: Alg g (Matcher g)
popMatcher = do
    cur <- uses matchers head
    modifying matchers tail
    return cur

lookupMapping :: Node -> Alg g Node
lookupMapping node = do
    curMappings <- use mappings
    return (S.index curMappings node)

isUsed :: Node -> Alg g Bool
isUsed node = isJust . S.elemIndexL node <$> use mappings


