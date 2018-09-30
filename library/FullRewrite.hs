{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
module FullRewrite where
import Types
import SpanningTree
import QuickSI
import Patch

import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Map as M



rewrite :: (g ~ Gr.Gr v e, Graph g, IsUnweighted (NodeData g), Eq (NodeData g)) => g -> PatchAlg g a -> g -> g
rewrite p patch g = case matches p g of
    [] -> g
    (finding:_) -> runPatch patch g p finding

matches :: (g ~ Gr.Gr v e, Graph g, IsUnweighted (NodeData g), Eq (NodeData g)) => g -> g -> [M.Map PatternNode GraphNode]
matches p g = runQuickSI g matchMST
  where
    matchMST = runMST (const . const 0) (const 0) p

test :: Gr.Gr (P ()) () -> Gr.Gr (P ()) ()
test = rewrite pat patch
  where
    patch :: PatchAlg (Gr.Gr (P ()) ()) ()
    patch = do
        Ann (x1, y1) _ <- labelOf 0
        Ann (x2, y2) _ <- labelOf 1
        replaceNode [((), 0), ((), 1)] 2 (Ann (x1, y1+30) ()) [((), 0), ((), 1)]
pat :: Gr.Gr (P ()) ()
pat = G.mkGraph [(i, Ann (fromIntegral i,fromIntegral i) ()) | i <- [0..1]] [(0, 1, ()), (1, 0, ())]

