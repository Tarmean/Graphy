{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
{-# Language DefaultSignatures #-}
{-# Language MultiParamTypeClasses #-}
module QuickSIClass where
import Types
import Control.Lens
import qualified Data.Map as M
import Control.Monad.Trans (lift)
import qualified Data.Graph.Inductive as G
import Control.Monad.State
import Data.Coerce
import System.Random.Shuffle
import System.Random
import Control.Applicative



class (MonadGraph m, MNode m ~ GraphNode, Monad m, Alternative m) => MonadQuickSI m where
    type MatchLabel m
    isUsed :: GraphNode -> m Bool
    tryLookup :: PatternNode -> m GraphNode
    selectMatch :: PatternNode -> GraphNode -> m ()
    branch :: [a] -> m a
instance (MatchLabels a l, G.Graph g) => MonadQuickSI (Alg (g a b) l) where
    type MatchLabel (Alg (g a b) l) = l
    isUsed node =  uses mappings (any (==node) . M.elems)
    tryLookup node = do
        curMappings <- use mappings
        return (curMappings M.! node)
    selectMatch p g = modifying mappings (M.insert p g)
    branch = Alg . lift


class MonadGraph m  where
    type MNode m
    type MLabel m
    outDegree :: MNode m -> m Int
    lookupLabel :: MNode m -> m (MLabel m)
    neighbors :: MNode m -> m [MNode m]
    allNodes :: m [MNode m]
    hasEdge :: MNode m -> MNode m -> m Bool

    default outDegree :: (Coercible (MNode m) Int, G.Graph g, HasGraph s (g a b), MonadState s m)
        => MNode m -> m Int
    outDegree m = do
        g <- use graph
        return (G.outdeg g (coerce m))


    default hasEdge :: (Coercible (MNode m) Int, G.Graph g, HasGraph s (g a b), MonadState s m)
        => MNode m -> MNode m -> m Bool
    hasEdge m n = do
        g <- use graph
        return (G.hasEdge g (coerce m, coerce n))


    default lookupLabel :: (Coercible (MNode m) Int, G.Graph g, HasGraph s (g (MLabel m) b), MonadState s m)
        => MNode m -> m (MLabel m)
    lookupLabel node = do
        curGraph <- use graph
        case G.lab curGraph (coerce node) of
          Just d -> return d
          Nothing -> error "Graph node not found"

    default neighbors :: (Coercible (MNode m) Int, G.Graph g, HasGraph s (g (MLabel m) b), MonadState s m)
        => MNode m -> m [MNode m]
    neighbors node = do
        curGraph <- use graph
        return $ (\(_, o, _) -> coerce o) <$> G.out curGraph (coerce node)

    default allNodes :: (Coercible (MNode m) Int, G.Graph g, HasGraph s (g (MLabel m) b), MonadState s m)
        => m [MNode m]
    allNodes = uses graph  (coerce . G.nodes)


instance (G.Graph g) => MonadGraph (Alg (g a b) l) where
    type MNode (Alg (g a b) l) = GraphNode
    type MLabel (Alg (g a b) l) = a
    
class MatchLabels l r where
    checkLabels :: l -> r -> Bool
instance Eq a => MatchLabels (P a) a where
    checkLabels (Ann _ a) b = a == b

class MonadShuffle m where
     shuffle :: [a] -> m [a]
instance MonadShuffle (Alg g l) where
     shuffle ls = do
         gen <- use rng
         let (gen', gen'') = split gen
             ls' = shuffle' ls (length ls) gen''
         rng .= gen'
         return ls'

