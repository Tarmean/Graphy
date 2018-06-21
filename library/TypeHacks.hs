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
type family NodeLabel g where
    NodeLabel (g nlbl elbl) = nlbl
type family EdgeLabel g where
    EdgeLabel (g nlbl elbl) = elbl
type family BaseGraph g  where
    BaseGraph (g nlbl elbl) = g
type PackedGraph g = (BaseGraph g (NodeLabel g) (EdgeLabel g) ~ g)
type Graphy g = (G.Graph (BaseGraph g), PackedGraph g)
type Dyn g = (G.DynGraph (BaseGraph g))

-- Pack `G.Graph g => g (Weighted a) (Weighted b)` into `WeightedGraph g => g`
type WeightedGraph g = (Dyn g, Graphy g, IsWeighted (NodeLabel g), IsWeighted (EdgeLabel g))

type family GetLabel l where
    GetLabel (Weighted l) = l
    GetLabel l = l

type IsWeighted a = (a ~ Weighted (GetLabel a), GetLabel (GetLabel a) ~ GetLabel a)

class HasLabel l  where
    getLabel :: l -> GetLabel l
instance  {-# Overlapping #-} (GetLabel (Weighted l) ~ l) => HasLabel (Weighted l) where
    getLabel (Weighted _ a) = a
instance {-# Overlappable #-}(GetLabel l ~ l) => HasLabel l where
    getLabel w = w

getWeight :: Weighted a -> Double
getWeight a = wWeight a
