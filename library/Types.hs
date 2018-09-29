{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language StandaloneDeriving #-}
{-# Language MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module Types (module Types, module TypeHacks)where

import qualified Data.Graph.Inductive as G
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Control.Applicative (Alternative)
import Control.Monad.State
import qualified Control.Monad.Logic as L
import Control.Lens.TH
import qualified Data.Map as M
import Graphics.Gloss.Data.ViewPort
import Control.Lens (Lens')


import TypeHacks
 
newtype PatternNode = PatternNode { unPNode :: G.Node }
  deriving (Eq, Ord, Num, Show)
newtype GraphNode = GraphNode { unGNode :: G.Node }
  deriving (Eq, Ord, Show, Num)

-- FIXME
makeGraph :: [(Int, Int)] -> G.Gr () ()
makeGraph xs =  graph0
  where
    uniqNodes = S.toList $ S.fromList $ concat [[x, y] | (x, y) <- xs]
    graph0 = G.mkGraph [(n, ()) | n <- uniqNodes] $ concat [[(n, m, ()), (m, n, ())] | (n, m) <- xs]

type AnnotatedEdge g = G.LEdge (EdgeData g)
type WeightedEdge g = WeightedGraph g => G.LEdge (EdgeData g)
type WithWeights g = BaseGraph g (Weighted (NodeData g)) (Weighted (EdgeData g))

type Node = Int
type Matcher g = NodeMatcher (GetLabel (NodeData g))
data NodeMatcher a
    = NodeMatcher 
    { parent :: Maybe Node
    , matcherLabel :: a
    , constraints :: [Constraint]
    , source :: PatternNode
    }
    deriving (Eq, Show)
data Constraint = Degree !Int | HasEdge !Node
  deriving (Eq, Show)

data MstEnv g
    = MstEnv
    { _mstEnvEdges :: S.Set G.Edge
    , _mstEnvVerts :: S.Set G.Node
    , _mstEnvMatchers :: [Matcher g]
    , _mstEnvGraph :: g
    }
makeFields ''MstEnv

runMstMonad :: MstMonad g a -> g -> (a, MstEnv g)
runMstMonad m = runState (unMST m) . MstEnv mempty mempty mempty

newtype MstMonad g a = MstMonad { unMST :: State (MstEnv g) a} deriving (Functor, Applicative, Monad, MonadState (MstEnv g))
type MST g a = (WeightedGraph g) => MstMonad g a

data QuickSIEnv g
    = QuickSIEnv
    { _quickSIEnvGraph :: g
    , _quickSIEnvMappings :: Seq.Seq Node
    , _quickSIEnvMatchers :: [Matcher g]
    }
makeFields ''QuickSIEnv
newtype Alg g a
    = Alg { runAlg :: StateT (QuickSIEnv g) [] a }
    deriving (Functor, Applicative, Monad, MonadState (QuickSIEnv g), Alternative, MonadPlus)
type ALG g a = (Graph g) => Alg g a

liftLs :: [a] -> Alg g a
liftLs = Alg . lift

toLogicT :: [a] -> L.LogicT m a
toLogicT ls = L.LogicT $ \cons zero -> foldr cons zero ls

type OriginPoint = (Float, Float)
data UIState
    = SClickedNode G.Node OriginPoint
    | SDraggingNode G.Node OriginPoint
    | SBase
    | SScaling OriginPoint
    | STranslating OriginPoint
data GlossState g
    = GlossState
    { _glossStateNodes :: M.Map G.Node (Float, Float)
    , _glossStateGraph :: g
    , _glossStateViewPort :: ViewPort
    , _glossStateSelected :: Maybe G.Node
    , _glossStateUiState :: UIState
    }

viewTranslate :: Lens' ViewPort (Float, Float)
viewTranslate f v = (\t' -> v {viewPortTranslate = t'}) <$> f (viewPortTranslate v)
viewScale :: Lens' ViewPort Float
viewScale f v = (\t' -> v {viewPortScale = t'}) <$> f (viewPortScale v)
makeFields ''GlossState

data RewriteEnv g
  = RewriteEnv
  { _rewriteEnvGraph :: g
  , _rewriteEnvMappings :: M.Map PatternNode GraphNode
  , _rewriteEnvPattern :: g
  , _rewriteEnvMaxKey :: GraphNode
  }
makeFields ''RewriteEnv
newtype PatchAlg g a
    = PatchAlg
    { unPatch :: State (RewriteEnv g) a
    } deriving (Monad, Functor, Applicative, MonadState (RewriteEnv g))
