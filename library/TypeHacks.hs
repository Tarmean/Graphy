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

type family NodeLabel g where
    NodeLabel (g nlbl elbl) = nlbl
type family EdgeLabel g where
    EdgeLabel (g nlbl elbl) = elbl
type family BaseGraph g  where
    BaseGraph (g nlbl elbl) = g
type PackedGraph g = (BaseGraph g (NodeLabel g) (EdgeLabel g) ~ g)
type Graphy g = (G.Graph (BaseGraph g), PackedGraph g)
type Dyn g = (G.DynGraph (BaseGraph g))

type family GetLabel l where
    GetLabel (Weighted l) = l
    GetLabel l = l

type IsWeighted a = (a ~ Weighted (GetLabel a), GetLabel (GetLabel a) ~ GetLabel a)

type WeightedGraph g = (Dyn g, Graphy g, IsWeighted (NodeLabel g), IsWeighted (EdgeLabel g))

type family CheckWeighted w where
    CheckWeighted (Weighted a) = 'True
    CheckWeighted _ = 'False
class LabelledApp (i::Bool) l  where
    getLabelApp :: l -> GetLabel l
instance (GetLabel l ~ l) => LabelledApp 'False l where
    getLabelApp w = w
instance LabelledApp 'True (Weighted i) where
    getLabelApp (Weighted _ a) = a

getLabel :: forall l. (LabelledApp (CheckWeighted l) l) => l -> GetLabel l
getLabel l = getLabelApp @(CheckWeighted l) l
getWeight :: IsWeighted a => a -> Double
getWeight a = wWeight a
