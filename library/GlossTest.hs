{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
module GlossTest where

import Data.Monoid
import Types
import Graphics.Gloss as Gr
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M
import Data.Function

main :: IO ()
main =  play (InWindow "Nice Window" (200, 200) (10, 10)) white 30 initialState render stepEvent stepTime
  where
    initialState = makeGraph [(0, (1, 2)), (1, (10, 10))]
    render = drawState
    stepEvent event state = state
    stepTime delta state = state

   

makeGraph :: [(Node, Point)] -> GlossState (G.Gr () ())
makeGraph xs =  GlossState locs graph0
  where
    locs = M.fromList xs
    graph0 = G.mkGraph [(n, ()) | (n, _) <- xs] [(n, m, ()) | (n, _) <- xs, (m, _) <- xs]


drawState :: Graph g => GlossState g -> Picture
drawState = drawNodes <> drawEdges

drawNodes :: GlossState g -> Picture
drawNodes state = pictures [ drawPoint point | point <- M.elems (stateNodes state) ]

drawEdges :: Graph g => GlossState g -> Picture
drawEdges g = pictures [ drawEdge g l r | (l, r) <- G.edges (stateGraph g)]

drawNode :: GlossState g -> G.Node -> Picture
drawNode g = drawPoint . pos g

drawPoint :: Point -> Picture
drawPoint (x, y) = translate x y nodePic
   where nodePic = ThickCircle 3 6

drawEdge :: GlossState g -> G.Node -> G.Node -> Picture
drawEdge g = drawLine `on` pos g

pos :: GlossState g -> G.Node -> Point
pos g n = stateNodes g M.! n

drawLine :: Point -> Point -> Picture
drawLine l r =  line [l, r]
