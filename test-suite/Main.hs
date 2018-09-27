{-# Language FlexibleContexts, TypeFamilies  #-}
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import SpanningTree
import QuickSI
import TypeHacks
import Types hiding (makeGraph)
import qualified Data.Graph.Inductive as G

andFlip :: [(a, a, c)] -> [(a, a, c)]
andFlip ls = ls ++ [(j, i, l) | (i, j, l) <- ls]
exGraph :: G.Gr Bool ()
exGraph = G.mkGraph [(i, i == 1) | i <- [0..4]] $ andFlip [(0, 1, ()), (1, 2, ()), (2, 3, ()), (2, 4, ())] 

exPattern :: G.Gr Bool ()
exPattern = G.mkGraph [(n,n == 3) | n <-[0..3]] $ andFlip [(0, n, ()) | n <- [1..3]]

-- foo = run exGraph exPattern

run :: (Graph g, Eq (GetLabel (NodeData g)), IsUnweighted (NodeData g)) => g -> [Matcher g] -> [[GraphNode]]
run g p = runQuickSI g p

mkOrder :: (Graph g, Eq (GetLabel (NodeData g)), IsUnweighted (NodeData g)) => g ->  [Matcher g]
mkOrder = runMST (\x y -> fromIntegral $ x + y) (\x -> fromIntegral x)


main :: IO ()
main = do
    test <- testSpec "graphgenerator" spec
    Test.Tasty.defaultMain test

-- spec :: Spec
-- spec = parallel $ do
--     it "is trivially true" $ do
--         True `shouldBe` True

spec :: Spec
spec = parallel $ do 
  it "traversal order should follow weights" $ do
    mkOrder pat `shouldBe` 
         [ NodeMatcher
             { parent = Nothing 
             , matcherLabel = 0 
             , constraints = [Degree 3]
             }
         , NodeMatcher
             { parent = Just 0 
             , matcherLabel = 1 
             , constraints = []
             }
         , NodeMatcher
             { parent = Just 0 
             , matcherLabel = 2 
             , constraints = []
             }
         , NodeMatcher
             { parent = Just 0 
             , matcherLabel = 3 
             , constraints = []
             }
         ]
  where
    pat :: G.Gr Int Int
    pat = G.mkGraph [(n,n) | n <-[0..3]] [(0, n, n) | n <- [1..3]]
