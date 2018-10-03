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
import Graphics.Gloss.Interface.Pure.Game as I
import Control.Lens
import GenMonad (runGenMonad, placeNode, (.->))
import ForceDirectedGraphLayout
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import System.Random

data Attrib = Treasure | Key | Lock | Encounter | Shop | Traps | Monsters | Start | End
  deriving (Show, Eq)
main :: IO ()
main =  do
    gen <- getStdGen
    play (InWindow "Nice Window" (200, 200) (10, 10)) white 30 (initialState gen) render processEvent stepTime
  where
    foo :: StdGen -> G.Gr (P [Attrib]) () -> G.Gr (P [Attrib]) ()
    foo gen g = runGenMonad gen g placeNode $ do
        G.empty .-> 1 `has` [Start] * 2 + 3 * 2 + 4 `has` [End]* 3 
        1 .-> 1 * (2+3)
            + 4 `has` [Treasure] * (2+3)
            + 2 * 5 `has` [Lock]
            + 3 `has` [Encounter] * 6 `has` [Key]
            + 5 * 6
        1 * 2 + 2 * 3 .-> 1 * 2 + 2 * 3 + 2 * 4 `has` [Shop]
        1 * 2 .-> 1 * (3 + 4)
                + 2 * (3 + 4)
                + 3 `has` [Monsters]
                + 4 `has` [Traps]
        -- 1 * 2 + 2 * 3 .-> 1 * 2 * 3
    has :: G.DynGraph g => Int -> [l] -> g [l] e
    has n l = ([], n, l, []) G.& G.empty


    initialState gen = over graph (foo gen) makeState
    render = drawState
    stepTime delta state
        = state & graph %~ stepNodes delta
                & placeDraggedNode 

f :: G.Gr (P l) b -> GenMonad (G.Gr (P l) b) l () -> IO (G.Gr (P l) b)
f g m = do
    gen <- getStdGen
    return $ runGenMonad gen g placeNode m

processEvent :: (Graph g, NodeData g ~ P [Attrib]) => Event -> GlossState g -> GlossState g
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

findNode :: G.Graph g => Vector -> GlossState (g (P [Attrib]) b) -> Maybe (G.Node, Float)
findNode pos state = find inCircle $ map labelDist candidates
  where
    adjustedPos = adjustPoint state pos
    labelDist  = over _2 (calcDist adjustedPos . getAnn)
    inCircle (_, dist) = dist <= 6
    candidates = state ^. graph . to G.labNodes

makeState ::  GlossState (G.Gr (P [Attrib]) ())
makeState =  GlossState G.empty viewPortInit Nothing SBase

drawState :: G.Graph g => GlossState (g (P [Attrib]) b) -> Picture
drawState =  applyPort <*> drawNodes <> drawEdges -- check if this is too cute once i am less tired
  where applyPort = view $ viewPort . to applyViewPortToPicture

drawNodes :: G.Graph g => GlossState (g (P [Attrib]) b) -> Picture
drawNodes state = pictures [ draw  w | w <- state ^. graph . to G.labNodes ]
  where
    isSelected n = state ^. selected == Just n
    draw (n,  Ann (x, y) l) = translate x y  $ drawPoint (isSelected n) l <> drawText l

drawEdges :: (G.Graph g) => GlossState (g (P [Attrib]) b) -> Picture
drawEdges g = pictures [ drawEdge g l r | (l, r) <- g ^. graph . to G.edges]


drawText :: Show s => s -> Picture
drawText w = translate 7 (-1) $ scale 0.1 0.1 $ text (show w)
drawPoint :: Bool -> [Attrib] -> Picture
drawPoint isSelected ls = colored nodePic
   where
     nodePic = ThickCircle 3 6
     colored
       | isSelected = color red
       | any isMeta ls = color (greyN 0.8)
       | otherwise = id

isMeta :: Attrib -> Bool
isMeta k = k == Key || k == Lock

drawEdge :: G.Graph g => GlossState (g (P [Attrib]) b) -> G.Node -> G.Node -> Picture
drawEdge g n1 n2 = drawLine (toPos n1) (toPos n2) (style n1 n2)
  where
    toPos = getPos (g^. graph)
    style = (||) `on` any isMeta . getLabel
    getLabel n
      | Just (Ann _ l) <- G.lab (g ^. graph) n = l
      | otherwise = error "Unknown node"


drawLine :: Point -> Point -> Bool -> Picture
drawLine l r b =  col $ line [l, r]
  where
    col
      | b = color (greyN 0.6)
      | otherwise = id


placeDraggedNode :: (G.DynGraph g, HasAnn n Point) => GlossState (g n e) -> GlossState (g n e)
placeDraggedNode state
  | SDraggingNode n p <- state ^. uiState = state & graph . graphNode n . lab . ann .~ p
  | otherwise = state
