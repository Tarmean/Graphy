{-# Language TypeFamilies #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}

{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language AllowAmbiguousTypes #-}

{-# Language DataKinds #-}
{-# Language InstanceSigs #-}
{-# Language UndecidableInstances #-}
{-# Language TypeOperators #-}
module TypeHacks where

import qualified Data.Graph.Inductive as G
import Control.Lens


-- The point of this module is to make type errors terrible
--
-- PackedGraph stuffs node and edge label types into type families so that we don't
-- have to mention them in type signatures that don't use them
--
-- Weights are tacked onto labels but functions that use labels shouldn't care whether a graph is weighted
-- So label uses HasLabel to work for both weighted and unweighted graphs
--
-- IsWeighted / Unweighted are sufficient to dispatch HasLabel, that's why the are defined weirdly

data Weighted a = Weighted { wWeight :: !Double, wLabel :: a }
  deriving Show

type family NodeData g where
    NodeData (g nlbl elbl) = nlbl
type family EdgeData g where
    EdgeData (g nlbl elbl) = elbl
type family BaseGraph g  where
    BaseGraph (g nlbl elbl) = g
type PackedGraph g = (BaseGraph g (NodeData g) (EdgeData g) ~ g)
type Graph g =
     ( G.DynGraph (BaseGraph g)
     , HasLabel (NodeData g)
     , HasLabel (EdgeData g)
     , PackedGraph g
     )
type WeightedGraph g = (Graph g, IsWeighted (NodeData g), IsWeighted (EdgeData g))

type family GetLabel l where
    GetLabel (Weighted l) = l
    GetLabel l = l

type IsWeighted a = (a ~ Weighted (GetLabel a))
type IsUnweighted a = (a ~ GetLabel a)

getWeight :: Weighted a -> Double
getWeight = wWeight


label :: forall l. HasLabel l => Lens' l (GetLabel l)
label = getLabelApp @(ShouldStripWeight l)

type HasLabel l = Labelled (ShouldStripWeight l) l

class Labelled (b::Bool) s  where
    getLabelApp :: Lens' s (GetLabel s)
instance Labelled 'True (Weighted l) where
    getLabelApp f w= (\a' -> w { wLabel = a' }) <$> f (wLabel w)
instance (GetLabel l ~ l) => Labelled 'False l where
    getLabelApp f a = f a
type family ShouldStripWeight l where
    ShouldStripWeight (Weighted l) = 'True
    ShouldStripWeight l = 'False
