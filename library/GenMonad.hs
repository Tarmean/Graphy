{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
module GenMonad where
import TypeHacks

import Types
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Map.Merge.Strict as MS
import SpanningTree (runMST)
import QuickSI (runQuickSI)
import Control.Lens
import System.Random
import Control.Monad.State (execState, runState)
import UnsafeNextGraphId
import Data.Coerce
import QuickSIClass
import Data.Function (on)

runGenMonad :: StdGen -> Gr.Gr a b -> (l -> ModifyMonad (Gr.Gr a b) l a) -> GenMonad (Gr.Gr a b) l () -> Gr.Gr a b
runGenMonad gen g0 lifter m = view graph $ execState (unGenMonad m) (GenEnv gen (unGNode $ nextId g0) g0 lifter)

placeNode :: l -> ModifyMonad g l (P l)
placeNode l = do
    x <- num
    y <- num
    return (Ann (x, y) l)
  where
    num = do
      gen <- use rng
      let (a, gen') = randomR (0, 100) gen
      rng .= gen'
      return a

infixr 1 .->
(.->)  :: (Monoid e, Semigroup n2, Ord e, MatchLabels n2 n1) =>
     Gr.Gr n1 e -> Gr.Gr n1 e -> GenMonad (Gr.Gr n2 e) n1 ()
(.->) l r = rewriting l (removeMissing l r >> insert r)

instance (Monoid m, Monoid n) => Num (Gr.Gr m n) where
    fromInteger n = G.mkGraph [(fromInteger n, mempty)] []
    (*) a b = G.mkGraph (mergeNodes a b) (G.labEdges a ++ G.labEdges b ++ newEdges)
      where newEdges = concat [[(l, r, mempty), (r, l, mempty)] | l<- G.nodes a, r <- G.nodes b]
    (+) = graphMerge
    signum = const G.empty
    abs = id
    negate = id

graphMerge :: (Semigroup a, G.Graph gr1, G.Graph gr2) => gr2 a b -> gr2 a b -> gr1 a b
graphMerge a b = G.mkGraph (mergeNodes a b) (G.labEdges a ++ G.labEdges b)


mergeNodes :: (Semigroup a, G.Graph g) => g a b -> g a b -> [(Node, a)]
mergeNodes = M.toList .: merge `on` (M.fromList . G.labNodes)
  where
    (.:) = (.).(.)

    merge :: (Ord a, Semigroup b) => M.Map a b -> M.Map a b -> M.Map a b
    merge = MS.merge MS.preserveMissing MS.preserveMissing $ MS.zipWithMatched whisk
    whisk _k x y = x <> y

getGen :: GenMonad r l StdGen
getGen = do
    gen <- use rng
    let (gen', gen'') = split gen
    rng .= gen''
    return gen'

rewriting
    :: (G.DynGraph g, MatchLabels n2 n1)
    => g n1 e2 -> ModifyMonad (g n2 e2) n1  () -> GenMonad (g n2 e2) n1 ()
rewriting p m = do
    gen <- getGen
    g <- use graph
    k <- use maxKey
    let searchOrder = runMST (const . const 0) p
        foundSubgraphIsos = runQuickSI gen g searchOrder
    case foundSubgraphIsos of
        [] -> return ()
        (translation:_) -> do
          gen' <- getGen
          lblMaker <- use liftLabel
          let ((), ModifyEnv _ k' g' gen'' _) = runState (unModifyMonad m) (ModifyEnv translation k g gen' lblMaker)
          maxKey .= k'
          graph .= g'
          rng .= gen''

insert :: (Semigroup a, Monoid b) => Gr.Gr l b -> ModifyMonad (Gr.Gr a b) l ()
insert g = do
    g' <- mapGraph g
    modifying graph (`graphMerge` g')

missingNodes :: Gr.Gr l b -> Gr.Gr l b -> [PatternNode]
missingNodes lhs rhs = S.toList $ S.fromList (coerce $ G.nodes lhs) S.\\ S.fromList (coerce $ G.nodes rhs)

missingEdges :: Ord b => Gr.Gr l b -> [(PatternNode, PatternNode, b)]
missingEdges lhs = coerce $ G.labEdges lhs

removeMissing :: forall a l b. (Ord b) => Gr.Gr l b -> Gr.Gr l b -> ModifyMonad (Gr.Gr a b) l ()
removeMissing p r = do
    let
      e :: [(Node, Node, b)]
      e = coerce $ missingEdges p
      v :: [Node]
      v = coerce $ missingNodes p r
    e' <- traverseOf (each . _1) translate e
    e'' <- traverseOf (each . _2) translate e'
    v' <- traverse translate v
    modifying graph (G.delNodes v')
    modifying graph (\g -> foldr G.delLEdge g e'')


mapGraph :: DynGraph g => BaseGraph g l (EdgeData g) -> ModifyMonad g l g
mapGraph = G.ufold step (pure G.empty)
  where step c g = (G.&) <$> translateContext c <*> g

translateContext :: G.Context l (EdgeData g) -> ModifyMonad g l (G.Context (NodeData g) (EdgeData g))
translateContext (inE, n, l, outE) = do
    inE' <- traverseOf (each . _2) translate inE
    outE' <- traverseOf (each . _2) translate outE
    mkLabel <- use liftLabel
    l' <- mkLabel l
    n' <- translate n
    return (inE', n', l', outE')

translate :: Node -> ModifyMonad g l Node
translate node = do
    m <- use mappings
    case M.lookup (PatternNode node) m of
        Just v -> return (unGNode v)
        Nothing -> do
           new <- use maxKey
           modifying maxKey (+1)
           modifying mappings (M.insert (PatternNode node) (GraphNode new))
           return new
