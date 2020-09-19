module GraphTraversalSpec
  (
    spec
  ) where

--------------------------------------
-- |
-- Module : GraphTraversalSpec.hs
-- Unit testing for the implementation
-- of the graph traversal algorithms
--
-- author tknawara
--
--------------------------------------
import qualified Data.Map.Strict      as Map
import           GraphRepresentation
import           GraphTraversal
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
  describe "DFS Tests" $ it "dfs sanity test" $ do
    let adjList = [[2, 3], [5], [4], [], []]
    g <- buildGraph adjList
    let expected = [1, 2, 5, 3, 4]
    actual <- dfs g vid (g Map.! 1)
    actual `shouldBe` expected

  describe "BFS Tests" $ it "bfs sanity test" $ do
    let adjList = [[2, 3], [5], [4], [], []]
    g <- buildGraph adjList
    let expected = [1, 2, 3, 5, 4]
    actual <- bfs g vid (g Map.! 1)
    actual `shouldBe` expected
