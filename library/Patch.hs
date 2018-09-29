{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
module Patch where
import TypeHacks
import Types
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M


class Patch m where
    type EdgeLabel m
    replaceEdges :: [(Node, Node, EdgeLabel m)] -> m ()
    deleteNode :: Node -> m ()
    addNode :: m Node

instance Patch (PatchAlg (g e v)) where
    -- type EdgeLabel (PatchAlg g) = (EdgeData g)
