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
import           GraphAlgorithms     (bfs, dfs)
import           GraphRepresentation (Vertex (..), buildGraph, resetGraph)

main :: IO ()
main = do
  let adjList = [[2, 3], [5], [4], [], []]
  g <- buildGraph adjList
  bfsOrder <- bfs g vid (g Map.! 1)
  resetGraph g
  dfsOrder <- dfs g vid (g Map.! 1)
  putStr "BFS visit order: "
  print bfsOrder
  putStr "DFS visit order: "
  print dfsOrder
