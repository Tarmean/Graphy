{-# Language FlexibleContexts #-}
module ForceDirectedGraphLayout where
import qualified Data.Graph.Inductive as G
import Graphics.Gloss as Gr
import Types
import Control.Lens
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

stepNodes :: G.DynGraph gr => Float -> gr (P l) b  -> gr (P l) b
stepNodes delta g = G.gmap (\(p, v, l, s) -> (p, v, step v l, s)) g
  where step p = over ann (stepSingle g delta p)

stepSingle :: G.Graph gr => gr (P l) b -> Float -> G.Node -> Point -> Point
stepSingle g delta point curPos
    = pushes V.+ pulls V.+ curPos
  where
      pushes = vSum [ delta `mulSV` push curPos otherPos | otherPos <- map (getAnn . snd) $ G.labNodes g]
      pulls = vSum [delta `mulSV` pull curPos otherPos | (_,other,_) <- G.out g point, let otherPos = getPos g other]
      vSum :: [ Point] -> Point
      vSum = foldl (V.+) (0,0)

pull :: Point -> Point -> Point
pull p1 p2 = 0.5 `mulSV` (p2 V.- p1)

push :: Point -> Point -> Point
push p1 p2
  | distSquared > 0 = pushForce  `mulSV` normalizeV (p1 V.- p2)
  | otherwise = v0
  where
    distSquared = calcDist p1 p2 ** 2
    pushForce = maxPush / distSquared
    maxPush = 100000
    
v0 :: Point
v0 = (0,0)

calcDist :: Point -> Point -> Float
calcDist p1 p2 =  magV (p2 V.- p1)

