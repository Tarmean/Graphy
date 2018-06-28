{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
module GlossTest where

import Data.Monoid
import Data.List (nub)
import Types
import Graphics.Gloss as Gr hiding (Point)
import Graphics.Gloss.Data.ViewState
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M
import Data.Function
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort

main :: IO ()
main =  play (InWindow "Nice Window" (200, 200) (10, 10)) white 30 initialState render stepEvent stepTime
  where
    initialState = makeGraph [(0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 0)]
    render = drawState
    stepEvent event state = state { viewState = updateViewStateWithEvent event (viewState state) }
    stepTime delta state = stepNodes state delta

   

makeGraph :: [(Int, Int)] -> GlossState (G.Gr () ())
makeGraph xs =  GlossState locs graph0 viewStateInit
  where
    nodes = nub $ concat $ [[x, y] | (x, y) <- xs]
    locs = M.fromList [(n, (fromIntegral n**3, (fromIntegral n * 8))) | n <- nodes]
    graph0 = G.mkGraph [(n, ()) | n <- nodes] $ concat [[(n, m, ()), (m, n, ())] | (n, m) <- xs]

stepNodes :: G.Graph gr => GlossState (gr a b) -> Float -> GlossState (gr a b)
stepNodes state delta = state { stateNodes = locs' }
  where
    locs' = M.mapWithKey (stepSingle state delta) (stateNodes state)

stepSingle :: G.Graph gr => GlossState (gr a b) -> Float -> G.Node -> Point -> Point
stepSingle (GlossState locs g _) delta point curPos = pushes + pulls + curPos
  where
      pushes = sum [ delta `mulSV` push curPos otherPos | otherPos <- M.elems locs]
      pulls = sum [delta `mulSV` pull curPos otherPos | (_,other,_) <- G.out g point, let otherPos = locs M.! other]

pull :: Point -> Point -> Point
pull p1 p2 = 0.5 `mulSV` (p2 - p1)

push :: Point -> Point -> Point
push p1 p2
  | distSquared > 0 = pushForce  `mulSV` normalizeV dist
  | otherwise = 0
  where
    dist = p1 - p2
    distSquared = magV dist ** 2
    pushForce = maxPush / distSquared
    maxPush = 1000000
    

drawState :: Graph g => GlossState g -> Picture
drawState =  applyPort <*> drawNodes <> drawEdges -- check if this is too cute once i am less tired
  where applyPort = applyViewPortToPicture . viewStateViewPort . viewState 

drawNodes :: GlossState g -> Picture
drawNodes state = pictures [ drawPoint point | point <- M.elems (stateNodes state) ]

drawEdges :: Graph g => GlossState g -> Picture
drawEdges g = pictures [ drawEdge g l r | (l, r) <- G.edges (stateGraph g)]

drawPoint :: Point -> Picture
drawPoint (x,y) = translate x y nodePic
   where
     nodePic = ThickCircle 3 6

drawEdge :: GlossState g -> G.Node -> G.Node -> Picture
drawEdge g = drawLine `on` pos g

pos :: GlossState g -> G.Node -> Point
pos g n = stateNodes g M.! n

drawLine :: Point -> Point -> Picture
drawLine l r =  line [l, r]
