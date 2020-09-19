module BellmanFordSpec
  (
    spec
  ) where

--------------------------------------
-- |
-- Module : BellmanFordSpec.hs
-- Unit testing for the implementation
-- of the bellman ford graph algorithms
--
-- author tknawara
--
--------------------------------------
import           BellmanFord
import qualified Data.Map.Strict      as Map
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
  describe "BellmanFord Tests" $ it "bellmanford sanity test" $ do
    let edges =
          [ Edge {src = 0, dest = 1, weight = 2}
          , Edge {src = 0, dest = 3, weight = 1}
          , Edge {src = 1, dest = 2, weight = 5}
          , Edge {src = 1, dest = 4, weight = 3}
          , Edge {src = 2, dest = 5, weight = 1}
          , Edge {src = 3, dest = 4, weight = 4}
          , Edge {src = 4, dest = 5, weight = 2}
          ]
    let actual = bellmanFord 6 0 edges
    let expected = Map.fromList
          [ (0, Only 0.0)
          , (1, Only 2.0)
          , (2, Only 7.0)
          , (3, Only 1.0)
          , (4, Only 5.0)
          , (5, Only 7.0)
          ]
    actual `shouldBe` expected
