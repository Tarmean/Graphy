{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
module GlossTest where

import Data.Monoid
import Data.List (nub, find)
import Types
import Graphics.Gloss as Gr hiding (Point)
import Graphics.Gloss.Data.ViewState
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M
import Data.Function
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens

main :: IO ()
main =  play (InWindow "Nice Window" (200, 200) (10, 10)) white 30 initialState render stepEvent stepTime
  where
    initialState = makeGraph [(0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 0)]
    render = drawState
    stepEvent =  processEvent
    stepTime delta state = stepDraggedNode (stepNodes state delta)

   
processEvent :: Event -> GlossState g -> GlossState g
processEvent (EventKey (MouseButton LeftButton) Down Modifiers {} pos) state 
    | Just (node, _) <- findNode pos state
      = state & uiState .~ SClicked node
processEvent (EventMotion pos') state 
    | SClicked n <- state ^. uiState = state & uiState .~ SDragging n adjustedPos
    | SDragging n _ <- state ^. uiState = state & uiState .~ SDragging n adjustedPos
    where adjustedPos = adjustPoint state pos'

processEvent (EventKey (MouseButton LeftButton) Up Modifiers {} _) state
  | SClicked n <- state ^. uiState = state & uiState .~ SBase
                                           & selected .~ Just n
  | otherwise = state & uiState .~ SBase

processEvent event state = state & viewState %~ updateViewStateWithEvent event

adjustPoint :: GlossState g -> Point -> Point
adjustPoint state pos = state ^. viewState . to viewStateViewPort . to invertViewPort  $ pos

findNode :: Vector -> GlossState g -> Maybe (G.Node, Float)
findNode pos state = find inCircle $ map labelDist candidates
  where
    adjustedPos = adjustPoint state pos
    labelDist  = over _2 (calcDist adjustedPos)
    inCircle (_, dist) = dist <= 6
    candidates = state ^. nodes . to M.toList

calcDist :: Point -> Point -> Float
calcDist p1 p2 =  magV (p2 - p1)

makeGraph :: [(Int, Int)] -> GlossState (G.Gr () ())
makeGraph xs =  GlossState locs graph0 viewStateInit Nothing SBase
  where
    uniqNodes = nub $ concat [[x, y] | (x, y) <- xs]
    locs = M.fromList [(n, (fromIntegral n**3, fromIntegral n * 8)) | n <- uniqNodes]
    graph0 = G.mkGraph [(n, ()) | n <- uniqNodes] $ concat [[(n, m, ()), (m, n, ())] | (n, m) <- xs]

stepNodes :: G.Graph gr => GlossState (gr a b) -> Float -> GlossState (gr a b)
stepNodes state delta = state & nodes %~ M.mapWithKey (stepSingle state delta)

stepDraggedNode :: GlossState g -> GlossState g
stepDraggedNode state
  | SDragging n p <- state ^. uiState = state & nodes . ix n .~ p
  | otherwise = state


stepSingle :: G.Graph gr => GlossState (gr a b) -> Float -> G.Node -> Point -> Point
stepSingle GlossState {_glossStateNodes=locs, _glossStateGraph=g} delta point curPos = pushes + pulls + curPos
  where
      pushes = sum [ delta `mulSV` push curPos otherPos | otherPos <- M.elems locs]
      pulls = sum [delta `mulSV` pull curPos otherPos | (_,other,_) <- G.out g point, let otherPos = locs M.! other]

pull :: Point -> Point -> Point
pull p1 p2 = 0.5 `mulSV` (p2 - p1)

push :: Point -> Point -> Point
push p1 p2
  | distSquared > 0 = pushForce  `mulSV` normalizeV (p1 - p2)
  | otherwise = 0
  where
    distSquared = calcDist p1 p2 ** 2
    pushForce = maxPush / distSquared
    maxPush = 1000000
    

drawState :: Graph g => GlossState g -> Picture
drawState =  applyPort <*> drawNodes <> drawEdges -- check if this is too cute once i am less tired
  where applyPort = view $ viewState . to viewStateViewPort . to applyViewPortToPicture

drawNodes :: GlossState g -> Picture
drawNodes state = pictures [ draw node x y | (node, (x, y)) <- state ^. nodes . to M.toList ]
  where
    isSelected n = state ^. selected == Just n
    draw n x y = translate x y  $ drawPoint (isSelected n)

drawEdges :: Graph g => GlossState g -> Picture
drawEdges g = pictures [ drawEdge g l r | (l, r) <- g ^. graph . to G.edges]


drawPoint :: Bool -> Picture
drawPoint isSelected = (colored nodePic)
   where
     nodePic = ThickCircle 3 6
     colored
       | isSelected = color red
       | otherwise = id

drawEdge :: GlossState g -> G.Node -> G.Node -> Picture
drawEdge g = drawLine `on` getPos g

getPos :: GlossState g -> G.Node -> Point
getPos g n = g ^?! nodes . ix n

drawLine :: Point -> Point -> Picture
drawLine l r =  line [l, r]
