{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
module FullRewrite where
import Types
import SpanningTree
import QuickSI
import Patch

import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.PatriciaTree as Gr



rewrite :: (g ~ Gr.Gr v e, Graph g, IsUnweighted (NodeData g), Eq (NodeData g)) => g -> PatchAlg g a -> g -> g
rewrite p patch g = case matches p g of
    [] -> g
    (finding:_) -> runPatch patch g p finding
matches p g = runQuickSI g matchMST
  where
    matchMST = runMST (const . const 0) (const 0) p

test :: Gr.Gr () () -> Gr.Gr () ()
test = rewrite pat patch
  where
    patch :: PatchAlg (Gr.Gr () ()) ()
    patch = do
        replaceNode [((), 0), ((), 3)] 2 () [((), 0), ((), 3)]
        replaceNode [((), 1), ((), 2)] 3 () [((), 1), ((), 2)]
pat = G.mkGraph [(i, ()) | i <- [0..1]] [(0, 1, ()), (1, 0, ())]

