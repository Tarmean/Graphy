{-# Language TypeFamilies #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}

{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language AllowAmbiguousTypes #-}

{-# Language DataKinds #-}
{-# Language UndecidableInstances #-}
{-# Language TypeOperators #-}
module TypeHacks where

import qualified Data.Graph.Inductive as G

data Weighted a = Weighted { wWeight :: !Double, wLabel :: a }

-- Pack `G.Graph g => g a b` into `Graphy g => g`
type family NodeData g where
    NodeData (g nlbl elbl) = nlbl
type family EdgeData g where
    EdgeData (g nlbl elbl) = elbl
type family BaseGraph g  where
    BaseGraph (g nlbl elbl) = g
type PackedGraph g = (BaseGraph g (NodeData g) (EdgeData g) ~ g)
type Graphy g = (G.Graph (BaseGraph g), PackedGraph g)
type Dyn g = (G.DynGraph (BaseGraph g))

-- Pack `G.Graph g => g (Weighted a) (Weighted b)` into `WeightedGraph g => g`
type WeightedGraph g = (Dyn g, Graphy g, IsWeighted (NodeData g), IsWeighted (EdgeData g))

type family GetLabel l where
    GetLabel (Weighted l) = l
    GetLabel l = l

type IsWeighted a = (a ~ Weighted (GetLabel a))

-- getLabel should strip Weighted and leave everything else alone
getLabel :: forall l. HasLabel l => l -> GetLabel l
getLabel l = getLabelApp @(ShouldStripWeight l) l
type HasLabel l = Labelled (ShouldStripWeight l) l
class Labelled (b::Bool) l  where
    getLabelApp :: l -> GetLabel l
instance  Labelled 'True (Weighted l) where
    getLabelApp (Weighted _ a) = a
instance (GetLabel l ~ l) => Labelled 'False l where
    getLabelApp w = w
type family ShouldStripWeight l where
    ShouldStripWeight (Weighted l) = 'True
    ShouldStripWeight l = 'False

getWeight :: Weighted a -> Double
getWeight a = wWeight a
