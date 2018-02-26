---------------------------------
-- |
-- Module : Main.hs
-- algorithms in haskell
-- Application entry point
--
-- author tarek-nawara
--
----------------------------------
module Main where

import qualified Data.Map.Strict      as Map
import           Dijkstra             (dijkstra)
import           WGraphRepresentation

main :: IO ()
main = do
  let adjList =
        [[(2, 2), (4, 1)], [(3, 5), (5, 3)], [(6, 1)], [(5, 4)], [(6, 2)], []]
  g <- buildWGraph adjList
  res <- dijkstra 1 g
  print res
