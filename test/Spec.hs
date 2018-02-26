--------------------------------------
-- |
-- Module : Spec.hs
-- Unit testing for the implementation
-- of the graph algorithms
--
-- author tarek-nawara
--
--------------------------------------
import qualified Data.Map.Strict     as Map
import           GraphAlgorithms
import           GraphRepresentation
import           Test.Hspec          (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner   (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "Dijkstra Tests" $ do
    it "dijkstra sanity test" $ do
      let adjList =
            [ [(2, 2), (4, 1)]
            , [(3, 5), (5, 3)]
            , [(6, 1)]
            , [(5, 4)]
            , [(6, 2)]
            , []
            ]
      g <- buildWGraph adjList
      let source = g Map.! 1
      actual <- dijkstra g source
      let expected =
            Map.fromList
              [ (1, Only 0.0)
              , (2, Only 2.0)
              , (3, Only 7.0)
              , (4, Only 1.0)
              , (5, Only 5.0)
              , (6, Only 7.0)
              ]
      actual `shouldBe` expected

  describe "DFS Tests" $ do
    it "dfs sanity test" $ do
      let adjList = [[2, 3], [5], [4], [], []]
      g <- buildGraph adjList
      let expected = [1, 2, 5, 3, 4]
      actual <- dfs g vid (g Map.! 1)
      actual `shouldBe` expected

  describe "BFS Tests" $ do
    it "bfs sanity test" $ do
      let adjList = [[2, 3], [5], [4], [], []]
      g <- buildGraph adjList
      let expected = [1, 2, 3, 5, 4]
      actual <- bfs g vid (g Map.! 1)
      actual `shouldBe` expected

      
