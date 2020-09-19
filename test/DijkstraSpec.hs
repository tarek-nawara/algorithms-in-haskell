module DijkstraSpec
  (
    spec
  ) where

--------------------------------------
-- |
-- Module : DijkstraSpec.hs
-- Unit testing for the implementation
-- of the dijkstra graph algorithms
--
-- author tknawara
--
--------------------------------------
import qualified Data.Map.Strict      as Map
import           Dijkstra
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)
import           WGraphRepresentation

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
  describe "Dijkstra Tests" $ it "dijkstra sanity test" $ do
    let adjList =
          [[(2, 2), (4, 1)], [(3, 5), (5, 3)], [(6, 1)], [(5, 4)], [(6, 2)], []]
    g      <- buildWGraph adjList
    actual <- dijkstra 1 g
    let expected = Map.fromList
          [ (1, Only 0.0)
          , (2, Only 2.0)
          , (3, Only 7.0)
          , (4, Only 1.0)
          , (5, Only 5.0)
          , (6, Only 7.0)
          ]
    actual `shouldBe` expected
