{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
module GlossTest where

import Data.List (find)
import Types
import Graphics.Gloss as Gr hiding (Point)
import Graphics.Gloss.Data.ViewPort
import qualified Data.Graph.Inductive as G
import Data.Function
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import GenMonad (runGenMonad, placeNode, (.->))
import ForceDirectedGraphLayout
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import System.Random

main :: IO ()
main =  do
    gen <- getStdGen
    play (InWindow "Nice Window" (200, 200) (10, 10)) white 30 (initialState gen) render processEvent stepTime
  where
    foo gen g = runGenMonad gen g placeNode $ do
        G.empty .-> 1 * 2 + 3 * 2 + 4 * 3
        1 * 2 .-> 3 * (1+2) + 4 * (1+2)
        1 * 2 .-> 1 * 2 * 3
        1 * 2 .-> 3 * (1+2) + 4 * (1+2)
        1 * 2 + 2 * 3 .-> 1 * 2 * 3

    initialState gen = over graph (foo gen) makeState
    render = drawState
    stepTime delta state
        = state & graph %~ stepNodes delta
                & placeDraggedNode 

f :: G.Gr (P ()) b -> GenMonad (G.Gr (P ()) b) () () -> IO (G.Gr (P ())
 b)
f g m = do
    gen <- getStdGen
    return $ runGenMonad gen g placeNode m
processEvent :: (Graph g, NodeData g ~ P ()) => Event -> GlossState g -> GlossState g
processEvent (EventKey (MouseButton LeftButton) Down Modifiers {} pos) state 
    | Just (node, _) <- findNode pos state
      = state & uiState .~ SClickedNode node pos
    | SBase <- state ^. uiState = state & uiState .~ STranslating pos
processEvent (EventKey (MouseButton RightButton) Down Modifiers {} pos) state 
    | SBase <- state ^. uiState = state & uiState .~ SScaling pos
processEvent (EventMotion pos') state 
    | SClickedNode n pos <- state ^. uiState
    , magV (pos V.- pos') >= 4
        = state & uiState .~ SDraggingNode n adjustedPos
    | SDraggingNode n _ <- state ^. uiState = state & uiState .~ SDraggingNode n adjustedPos
    | STranslating origin <- state ^. uiState
        = state
            & viewPort . viewTranslate %~ ((pos' V.- origin) V.+)
            &  uiState .~ STranslating pos'
                
    | SScaling origin <- state ^. uiState
        = state
            & viewPort . viewScale +~ valV (pos' V.- origin)
            & uiState .~ SScaling pos'
    where
      adjustedPos = adjustPoint state pos'
processEvent (EventKey (MouseButton LeftButton) Up Modifiers {} _) state
  | SClickedNode n _ <- state ^. uiState = state & uiState .~ SBase
                                           & selected .~ Just n
  | otherwise = state & uiState .~ SBase
processEvent (EventKey (MouseButton RightButton) Up Modifiers {} _) state
  | SScaling _  <- state ^. uiState = state & uiState .~ SBase
processEvent _ s = s

valV :: Point -> Float
valV (x, y) = (x+y) / 100

adjustPoint :: GlossState g -> Point -> Point
adjustPoint state =  invertViewPort (state ^. viewPort)

findNode :: G.Graph g => Vector -> GlossState (g (P ()) b) -> Maybe (G.Node, Float)
findNode pos state = find inCircle $ map labelDist candidates
  where
    adjustedPos = adjustPoint state pos
    labelDist  = over _2 (calcDist adjustedPos . getAnn)
    inCircle (_, dist) = dist <= 6
    candidates = state ^. graph . to G.labNodes

makeState ::  GlossState (G.Gr (P ()) ())
makeState =  GlossState G.empty viewPortInit Nothing SBase

drawState :: G.Graph g => GlossState (g (P ()) b) -> Picture
drawState =  applyPort <*> drawNodes <> drawEdges -- check if this is too cute once i am less tired
  where applyPort = view $ viewPort . to applyViewPortToPicture

drawNodes :: G.Graph g => GlossState (g (P ()) b) -> Picture
drawNodes state = pictures [ draw node x y w | (node, w@(Ann (x, y) _)) <- state ^. graph . to G.labNodes ]
  where
    isSelected n = state ^. selected == Just n
    draw n x y w = translate x y  $ drawPoint (isSelected n) <> drawText w

drawEdges :: (G.Graph g) => GlossState (g (P ()) b) -> Picture
drawEdges g = pictures [ drawEdge g l r | (l, r) <- g ^. graph . to G.edges]


drawText :: Show s => s -> Picture
drawText w = translate 7 (-1) $ scale 0.1 0.1 $ text (show w)
drawPoint :: Bool -> Picture
drawPoint isSelected = (colored nodePic)
   where
     nodePic = ThickCircle 3 6
     colored
       | isSelected = color red
       | otherwise = id

drawEdge :: G.Graph g => GlossState (g (P ()) b) -> G.Node -> G.Node -> Picture
drawEdge g = drawLine `on` getPos (g ^. graph)

drawLine :: Point -> Point -> Picture
drawLine l r =  line [l, r]


placeDraggedNode :: (G.DynGraph g, HasAnn n Point) => GlossState (g n e) -> GlossState (g n e)
placeDraggedNode state
  | SDraggingNode n p <- state ^. uiState = state & graph . graphNode n . lab . ann .~ p
  | otherwise = state
