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
import           GraphTraversal
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
  describe "DFS Tests" $ it "dfs sanity test" $ do
    let adj = Map.fromList [(1, [2, 3]), (2, [5]), (3, [4]), (4, []), (5, [])]
    let expected = [1, 2, 5, 3, 4]
    let actual   = dfs adj id 1
    actual `shouldBe` expected

  describe "BFS Tests" $ it "bfs sanity test" $ do
    let adj = Map.fromList [(1, [2, 3]), (2, [5]), (3, [4]), (4, []), (5, [])]
    let expected = [1, 2, 3, 5, 4]
    let actual   = bfs adj id 1
    actual `shouldBe` expected
