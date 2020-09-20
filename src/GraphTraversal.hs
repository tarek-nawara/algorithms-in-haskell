-------------------------------------
-- |
-- Module : GraphTraversal
-- algorithms in haskell
-- Implementation of graph traversal
-- algorithms in haskell.
--
-- author tarek-nawara
--
---------------------------------------
module GraphTraversal
  ( dfs
  , bfs
  ) where

import           Control.Monad
import qualified Data.Map.Strict     as Map

type AdjMatrix = Map.Map Int [Int]
data TravState = TravState (Map.Map Int Bool)

-- | Implementation of breadth first algorithm
--   given the graph and a source vertex.
bfs :: AdjMatrix -> (Int -> a) -> Int -> [a]
bfs adj consumer source =
  let state = TravState (Map.fromList [ (i, False) | i <- Map.keys adj ])
  in  bfsInner adj consumer [source] state

bfsInner :: AdjMatrix -> (Int -> a) -> [Int] -> TravState -> [a]
bfsInner _ _ [] _ = []
bfsInner adj consumer level state =
  let allNext            = concat (map (\v -> adj Map.! v) level)
      (TravState marked) = state
      filtered           = filter (\v -> not (marked Map.! v)) allNext
      filteredState      = Map.fromList [ (i, True) | i <- filtered ]
      newState           = TravState (filteredState `Map.union` marked)
      values             = map consumer level
  in  values ++ (bfsInner adj consumer filtered newState)

-- | Implementation of depth first algorithm
--   given the graph and a source vertex.
dfs :: AdjMatrix -> (Int -> a) -> Int -> [a]
dfs adj consumer source =
  let state = TravState (Map.fromList [ (i, False) | i <- Map.keys adj ])
  in  dfsInner adj consumer source state

dfsInner :: AdjMatrix -> (Int -> a) -> Int -> TravState -> [a]
dfsInner adj consumer source state =
  let (TravState marked) = state
      smarked            = marked Map.! source
      newState           = TravState (Map.insert source True marked)
  in  if smarked
        then []
        else (consumer source) : concat
          (map (\v -> dfsInner adj consumer v newState) (adj Map.! source))

