{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module Types (module Types, module TypeHacks)where

import qualified Data.Graph.Inductive as G
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Reader
import qualified Control.Monad.Logic as L
import Control.Lens.TH

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB


import TypeHacks

type AnnotatedEdge g = G.LEdge (EdgeData g)
type WeightedEdge g = WeightedGraph g => G.LEdge (EdgeData g)

type Node = Int
type Matcher g = NodeMatcher (GetLabel (NodeData g))
data NodeMatcher a
    = NodeMatcher 
    { parent :: Maybe Node
    , label :: a
    , constraints :: [Constraint]
    }
data Constraint = Degree !Int | HasEdge !Node

data MstEnv g
    = MstEnv
    { _mstEnvEdges :: S.Set G.Edge
    , _mstEnvVerts :: S.Set G.Node
    , _mstEnvMatchers :: [Matcher g]
    , _mstEnvGraph :: g
    }
makeFields ''MstEnv

newtype MstMonad g a = MstMonad (State (MstEnv g) a) deriving (Functor, Applicative, Monad, MonadState (MstEnv g))
type MST g a = (Dyn g, WeightedGraph g, HasLabel (NodeData g), HasLabel (EdgeData g)) => MstMonad g a

type PatternNode = G.Node 
type GraphNode = G.Node 
data QuickSIEnv s g
    = QuickSIEnv
    { _quickSIEnvGraph :: g
    , _quickSIEnvMappings :: VU.MVector s GraphNode
    , _quickSIEnvUsed :: VU.MVector s Bool
    , _quickSIEnvDepth :: Int
    , _quickSIEnvMatchers :: VB.Vector (Matcher g)
    }
makeFields ''QuickSIEnv
newtype Alg s g a
    = Alg (ReaderT (QuickSIEnv s g) (L.LogicT (ST s)) a)
    deriving (Functor, Applicative, Monad, MonadReader (QuickSIEnv s g), Alternative, MonadPlus)
type ALG s g a = (Graphy g, HasLabel (NodeData g), HasLabel (EdgeData g)) => Alg s g a

liftST :: ST s a -> Alg s g a
liftST = Alg . lift . lift

liftLs :: [a] -> Alg s g a
liftLs = Alg . lift . toLogicT
toLogicT :: [a] -> L.LogicT m a
toLogicT ls = L.LogicT $ \cons zero -> foldr cons zero ls
