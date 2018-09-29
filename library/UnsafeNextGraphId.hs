
module UnsafeNextGraphId where
import qualified Data.IntMap as IM
import Unsafe.Coerce
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import Types (GraphNode (..))

nextId :: Gr.Gr v e -> GraphNode
nextId g = case IM.lookupMax coerced of
    Nothing -> GraphNode 0
    Just (k, _) -> GraphNode (k + 1)


  where
    coerced :: IM.IntMap (IM.IntMap [e], v, IM.IntMap [e])
    coerced = unsafeCoerce g
