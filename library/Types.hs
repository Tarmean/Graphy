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
import Control.Applicative (Alternative)
import Control.Monad.State
import qualified Control.Monad.Logic as L
import Control.Lens.TH
import qualified Data.Map.Strict as M
import Graphics.Gloss.Data.ViewPort
import Control.Lens (Lens', Traversal')
import System.Random

import TypeHacks
 
data Ann a b = Ann 
  { _annAnn :: a
  , _annDat :: b
  }
instance Show b => Show (Ann a  b) where
    show (Ann _ b) = show b
instance Semigroup (Ann (Float, Float) ()) where
    Ann l1 r1 <> Ann l2 r2 = Ann (l1) (r1 <> r2)
instance Monoid (Ann (Float, Float) ()) where
    mempty = Ann (0,0) mempty
makeFields ''Ann
getAnn :: Ann a b -> a
getAnn (Ann a _) = a
type P b = Ann (Float, Float) b
newtype PatternNode = PatternNode { unPNode :: G.Node }
  deriving (Eq, Ord, Num, Show)
newtype GraphNode = GraphNode { unGNode :: G.Node }
  deriving (Eq, Ord, Show, Num)

-- FIXME

type WithWeights g = BaseGraph g (Weighted (NodeData g)) (Weighted (EdgeData g))

type Node = Int
type Matcher g = NodeMatcher (NodeData g)
data NodeMatcher a
    = NodeMatcher 
    { parent :: Maybe PatternNode
    , matcherLabel :: a
    , constraints :: [Constraint]
    , source :: PatternNode
    }
    deriving (Eq, Show)
data Constraint = Degree !Int | HasEdge !PatternNode
  deriving (Eq, Show)

data MstEnv g
    = MstEnv
    { _mstEnvEdges :: S.Set G.Edge
    , _mstEnvVerts :: S.Set G.Node
    , _mstEnvMatchers :: [NodeMatcher (NodeData g)]
    , _mstEnvGraph :: g
    }
makeFields ''MstEnv

runMstMonad :: MstMonad g a -> g -> (a, MstEnv g)
runMstMonad m = runState (unMST m) . MstEnv mempty mempty mempty

newtype MstMonad g a = MstMonad { unMST :: State (MstEnv g) a} deriving (Functor, Applicative, Monad, MonadState (MstEnv g))

data QuickSIEnv g l
    = QuickSIEnv
    { _quickSIEnvGraph :: g
    , _quickSIEnvMappings :: M.Map PatternNode GraphNode
    , _quickSIEnvRng :: StdGen
    }
makeFields ''QuickSIEnv
newtype Alg g l a
    = Alg { runAlg :: StateT (QuickSIEnv g l) [] a }
    deriving (Functor, Applicative, Monad, MonadState (QuickSIEnv g l), Alternative, MonadPlus)

liftLs :: [a] -> Alg g l a
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
    { _glossStateGraph :: g
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

getPos :: G.Graph g => g (P ()) b -> G.Node -> (Float, Float)
getPos g p = case G.lab g p of
    Nothing -> error "Point unknown"
    Just (Ann r _) -> r

data NContext n e
    = NContext
    { _nContextInEdges :: [(e, Node)]
    , _nContextNodeId :: Node
    , _nContextLab :: n
    , _nContextOutEdges :: [(e, Node)]
    }
makeFields ''NContext
graphNode :: G.DynGraph g => Node -> Traversal' (g n e)  (NContext n e)
graphNode node f g = case G.match node g of
    (Just (to, n, l, from), g') -> (\(NContext to' n' l' from') -> (to', n', l', from') G.& g') <$> f (NContext to n l from)
    (Nothing, g') -> pure g'


data ModifyEnv g l
    = ModifyEnv
    { _modifyEnvMappings :: M.Map PatternNode GraphNode
    , _modifyEnvMaxKey :: Int
    , _modifyEnvGraph :: g
    , _modifyEnvRng :: StdGen
    , _modifyEnvLiftLabel :: l -> ModifyMonad g l (NodeData g)
    }
newtype ModifyMonad g l a = ModifyMonad { unModifyMonad :: State (ModifyEnv g l) a }
    deriving (Monad, Applicative, Functor, MonadState (ModifyEnv g l))

data GenEnv g l
    = GenEnv
    { _genEnvRng :: StdGen
    , _genEnvMaxKey :: Int
    , _genEnvGraph :: g
    , _genEnvLiftLabel :: l -> ModifyMonad g l (NodeData g)
    }
newtype GenMonad g l a = GenMonad { unGenMonad :: State (GenEnv g l) a }
    deriving (Monad, Applicative, Functor, MonadState (GenEnv g l))
makeFields ''GenEnv
makeFields ''ModifyEnv

-- rewrites 
     -- a - b - c
-- into
     -- a - b - c
     --  \     /
     --   - d -
-- splitPath = do
--     rewriting ((1 + 2) * 3) $ do
--         insert ((1 + 2) * 4)
