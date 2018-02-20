-------------------------------------
-- |
-- Module : Lib
-- algorithms in haskell
-- Implementation of graph algorithms
-- in haskell.
-- author tarek-nawara
--
---------------------------------------
module Lib
  ( someFunc
  ) where

import           Data.IORef
import           Control.Monad
import           Data.Functor
import qualified Data.Map.Strict as Map

type Graph = Map.Map Int Vertex

-- | Representation of a vertex in a graph
data Vertex = Vertex
  { vid    :: Int
  , marked :: IORef Bool
  , adj    :: [Int]
  }

-- Get all the neighbor vertices to
-- a given vertex.
neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = map (\vid -> g Map.! vid) (adj v)

-- Change the marked state of a vertex
changeState :: Vertex -> IO ()
changeState v = modifyIORef (marked v) not

-- Check wither the vertex is marked or not
shouldVisit :: Vertex -> IO Bool
shouldVisit v = do
  isMarked <- readIORef (marked v)
  return (not isMarked)

-- Reset the state of all vertices of the graph
resetGraph :: Graph -> IO ()
resetGraph g = mapM_ (\(_, v) -> changeState v) (Map.toList g)
  
-- | Implementation of depth first algorithm
--   given the graph and a source vertex. 
dfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
dfs g consumer source = do
  changeState source
  children    <- filterM shouldVisit (neighbors g source)
  childrenRes <- mapM (dfs g consumer) children
  return $ consumer source : concat childrenRes

-- | Implementation of breadth first algorithm
--   given the graph and the source vertex.
bfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
bfs g consumer source = bfsInner g consumer [source]
  where
    bfsInner _ _ [] = return []
    bfsInner g consumer (source:rest) = do
      changeState source
      children <- filterM shouldVisit (neighbors g source)
      restRes  <- bfsInner g consumer (rest ++ children)
      return $ consumer source : restRes

-- | Build a graph from the adj list
buildGraph :: [[Int]] -> IO Graph
buildGraph l = Map.fromList <$> buildGraphInner 1 l
  where
    buildGraphInner _ [] = return []
    buildGraphInner vid (adj:rest) = do
      vMarked <- newIORef False
      let v =  Vertex { vid = vid, marked = vMarked, adj = adj }
      restG <- buildGraphInner (vid + 1) rest
      return $ (vid, v) : restG
  
-- TODO: this function should be removed
someFunc :: IO ()
someFunc = do
  let adjList = [[2, 3], [5], [4], [], []]
  g <- buildGraph adjList
  bfsOrder <- bfs g vid (g Map.! 1)
  resetGraph g
  dfsOrder <- dfs g vid (g Map.! 1)
  putStr "BFS visit order: "
  print bfsOrder
  putStr "DFS visit order: "
  print dfsOrder
  
