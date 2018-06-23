{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
module GlossTest where

import Data.Monoid
import Types
import Graphics.Gloss
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M
import Data.Function

main :: IO ()
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)



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
