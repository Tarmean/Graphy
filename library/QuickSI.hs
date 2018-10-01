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
import qualified Data.Map.Strict as M
import System.Random
import QuickSIClass


runQuickSI
    :: (G.DynGraph g, MatchLabels n l)
    => StdGen ->  g n e -> [NodeMatcher l] -> [M.Map PatternNode GraphNode]
runQuickSI gen g patt = map (view mappings) $ execStateT (runAlg $ algorithm patt) env
  where
    env = QuickSIEnv
        { _quickSIEnvGraph = g
        , _quickSIEnvMappings  = M.empty
        , _quickSIEnvRng = gen
        }

algorithm :: (MonadShuffle m, MonadQuickSI m, MatchLabels (MLabel m) (MatchLabel m)) => [NodeMatcher (MatchLabel m)] -> m ()
algorithm = foldM_ (const step) ()
  where step matcher = candidates matcher >>= uncurry selectMatch

candidates :: (MonadShuffle m, MonadQuickSI m, MatchLabels (MLabel m) (MatchLabel m)) => NodeMatcher (MatchLabel m) -> m (PatternNode, GraphNode)
candidates matcher = do
    node <- branch =<< shuffle =<< availableSuccessors matcher

    nodeUsed <- isUsed node
    guard (not nodeUsed)

    curLabel <- lookupLabel node
    guard $ checkLabels curLabel (matcherLabel matcher)

    guardAll (checkConstraint node) (constraints matcher)

    return (source matcher, node)
  where guardAll predicate ls = guard . and =<< traverse predicate ls

checkConstraint :: (MonadQuickSI m, MonadGraph m) => GraphNode -> Constraint -> m Bool
checkConstraint node (Degree n) = (>=n) <$> outDegree node
checkConstraint node (HasEdge other) = hasEdge node =<< tryLookup other

availableSuccessors :: (MonadQuickSI m, MonadGraph m) => NodeMatcher l -> m [GraphNode]
availableSuccessors matcher = do
    case parent matcher of
        Nothing -> allNodes
        Just curParent -> do
          previous <- tryLookup curParent
          neighbors previous
