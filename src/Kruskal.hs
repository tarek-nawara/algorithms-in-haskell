module Kruskal
  (
    kruskal
  , Edge(..)
  ) where


-------------------------------------
-- |
-- Module : Kruskal
-- algorithms in haskell
-- Implementation of kruskal algorithm in haskell
--

-- author tknawara
--
------------------------------------

import Control.Monad.State
import Data.Ord
import Data.Sort
import qualified Data.Map.Strict as Map

type Site = Int

data Edge = Edge
  { src :: Int
  , dest :: Int
  , weight :: Double
  } deriving (Show, Eq)

data UFState = UFState
  { relations :: Map.Map Site Site
  , size :: Map.Map Site Int
  }

-- | Implementation of kruskal
--   algorithm for finding MST
kruskal :: Int -> [Edge] -> Double
kruskal n edges =
  let initialRelations = Map.fromList ([ (i, i) | i <- [0 .. n] ])
      initialSize      = Map.fromList ([ (i, 1) | i <- [0 .. n] ])
      initialState     = (UFState initialRelations initialSize)
      sortedEdges      = sortOn (\e -> weight e) edges
      (_, total)       = foldl (\(st, total) edge -> (move st total edge))
                               (initialState, 0)
                               sortedEdges
  in  total
 where
  move st total edge =
    let (usage, newState) = runState (useEdge edge) st
    in  (newState, total + usage)


-- | Implementation of find algorithm as part
--   of union find, this implementation also
--   include path compression.
findParent :: Site -> State UFState Site
findParent vertex = do
  (UFState relations size) <- get
  let parent = relations Map.! vertex
  if parent == vertex
    then return vertex
    else do
      ancestor <- findParent parent
      let updated = Map.insert vertex ancestor relations
      put (UFState updated size)
      return ancestor


-- | Implementation of union algorithm as part
--   of union find.
unionSite :: Site -> Site -> State UFState ()
unionSite r1 r2 = do
  (UFState relations size) <- get
  let s1    = size Map.! r1
  let s2    = size Map.! r2
  let total = s1 + s2
  let (updatedRelations, updatedSize) = case (compare s1 s2) of
        LT -> ((Map.insert r1 r2 relations), (Map.insert r2 total size))
        GT -> ((Map.insert r2 r1 relations), (Map.insert r1 total size))
        EQ -> ((Map.insert r2 r1 relations), (Map.insert r1 total size))
  put (UFState updatedRelations updatedSize)
  return ()


-- Helper function to determine if
-- we need to use this edge in our MST or not.
useEdge :: Edge -> State UFState Double
useEdge e = do
  r1 <- findParent (src e)
  r2 <- findParent (dest e)
  if r1 /= r2
    then do
      _ <- unionSite r1 r2
      return (weight e)
    else return 0
