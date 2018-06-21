{-# Language TypeFamilies #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

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

data Weighted a = Weighted { wWeight :: !Double, wLabel :: a }

-- Pack `G.Graph g => g a b` into `Graph g => g`
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

-- Pack `G.Graph g => g (Weighted a) (Weighted b)` into `WeightedGraph g => g`

getWeight :: Weighted a -> Double
getWeight a = wWeight a

-- It's all downward from here
-- Label should use wLabel for Weighted labels and id otherwise

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
