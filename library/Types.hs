{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Types (module Types, module TypeHacks)where

import qualified Data.Graph.Inductive as G
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Reader
import qualified Control.Monad.Logic as L

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB


import TypeHacks

type Node = Int
data NodeMatcher a
    = NodeMatcher 
    { parent :: Maybe Node
    , label :: a
    , constraints :: [Constraint]
    }
data Constraint = Degree !Int | HasEdge !Node

type LabeledEdge g = G.LEdge (EdgeLabel g)
type WeightedEdge g = G.LEdge (Weighted (EdgeLabel g))

data MstEnv g
    = MstEnv
    { mstEdges :: S.Set G.Edge
    , mstVerts :: S.Set G.Node
    , mstSeq :: [NodeMatcher (NodeLabel g)]
    , mstGraph :: g
    }
newtype MstMonad g a = MstMonad (State (MstEnv g) a) deriving (Functor, Applicative, Monad, MonadState (MstEnv g))
type MST g a = (Dyn g, WeightedGraph g) => MstMonad g a

type PatternNode = G.Node 
type GraphNode = G.Node 
data QuickSIEnv s g
    = QuickSIEnv
    { envGraph :: g
    , envMappings :: VU.MVector s GraphNode
    , envUsed :: VU.MVector s Bool
    , envDepth :: Int
    , envMatchers :: VB.Vector (NodeMatcher (NodeLabel g))
    }
newtype Alg s g a
    = Alg (ReaderT (QuickSIEnv s g) (L.LogicT (ST s)) a)
    deriving (Functor, Applicative, Monad, MonadReader (QuickSIEnv s g), Alternative, MonadPlus)
type ALG s g a = Graphy g => Alg s g a
