module KruskalSpec
  (
    spec
  ) where

--------------------------------------
-- |
-- Module : KruskalSpec.hs
-- Unit testing for the implementation
-- of the bellman ford graph algorithms
--
-- author tknawara
--
--------------------------------------
import           Kruskal
import qualified Data.Map.Strict      as Map
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
  describe "Kruskal Tests" $ it "kruskal sanity test" $ do
    let edges =
          [ Edge {src = 0, dest = 1, weight = 1}
          , Edge {src = 0, dest = 2, weight = 1}
          , Edge {src = 0, dest = 3, weight = 1}
          , Edge {src = 2, dest = 3, weight = 10}
          ]
    let actual   = kruskal 4 edges
    let expected = 3
    actual `shouldBe` expected
