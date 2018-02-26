--------------------------------------
-- |
-- Module : Spec.hs
-- Unit testing for the implementation
-- of the graph algorithms
--
-- author tarek-nawara
--
--------------------------------------
import           BellmanFord
import qualified Data.Map.Strict      as Map
import           Dijkstra
import           GraphAlgorithms
import           GraphRepresentation
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)
import           WGraphRepresentation

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "Dijkstra Tests" $
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
      actual <- dijkstra 1 g
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

  describe "BellmanFord Tests" $
    it "bellmanford sanity test" $ do
      let adjList =
            [ [(2, 2), (4, 1)]
            , [(3, 5), (5, 3)]
            , [(6, 1)]
            , [(5, 4)]
            , [(6, 2)]
            , []
            ]
      let edges =
            [ Edge {src = 1, dest = 2, edgeWeight = 2}
            , Edge {src = 1, dest = 4, edgeWeight = 1}
            , Edge {src = 2, dest = 3, edgeWeight = 5}
            , Edge {src = 2, dest = 5, edgeWeight = 3}
            , Edge {src = 3, dest = 6, edgeWeight = 1}
            , Edge {src = 4, dest = 5, edgeWeight = 4}
            , Edge {src = 5, dest = 6, edgeWeight = 2}
            ]
      g <- buildWGraph adjList
      actual <- bellmanFord 1 g edges
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

  describe "DFS Tests" $
    it "dfs sanity test" $ do
      let adjList = [[2, 3], [5], [4], [], []]
      g <- buildGraph adjList
      let expected = [1, 2, 5, 3, 4]
      actual <- dfs g vid (g Map.! 1)
      actual `shouldBe` expected

  describe "BFS Tests" $
    it "bfs sanity test" $ do
      let adjList = [[2, 3], [5], [4], [], []]
      g <- buildGraph adjList
      let expected = [1, 2, 3, 5, 4]
      actual <- bfs g vid (g Map.! 1)
      actual `shouldBe` expected
