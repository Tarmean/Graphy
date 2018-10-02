module GenClass where
import Types
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Graph.Inductive as G

class MonadGen m where
    translate :: PatternNode -> m GraphNode
    (.->) :: (Monoid e, Ord e, G.DynGraph g)
          => g n1 e -> g n1 e -> m ()

