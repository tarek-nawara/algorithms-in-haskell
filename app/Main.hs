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

import qualified Data.Map.Strict     as Map
import           GraphAlgorithms     (bfs, dfs, dijkstra)
import           GraphRepresentation

-- TODO convert to unit tests.
dijkstraTest :: IO (Map.Map Int Weight)
dijkstraTest = do
  let adjList =
        [[(2, 2), (4, 1)], [(3, 5), (5, 3)], [(6, 1)], [(5, 4)], [(6, 2)], []]
  g <- buildWGraph adjList
  let source = g Map.! 1
  dijkstra g source

-- TODO convert to unit tests.
bfsTest :: IO [Int]
bfsTest = do
  let adjList = [[2, 3], [5], [4], [], []]
  g <- buildGraph adjList
  bfs g vid (g Map.! 1)

-- TODO convert to unit tests.
dfsTest :: IO [Int]
dfsTest = do
  let adjList = [[2, 3], [5], [4], [], []]
  g <- buildGraph adjList
  dfs g vid (g Map.! 1)

main :: IO ()
main = do
  res <- dijkstraTest
  print res
