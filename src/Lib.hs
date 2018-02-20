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
  
-- TODO: this function should be removed
someFunc :: IO ()
someFunc = do
  oneMarked    <- newIORef False
  twoMarked    <- newIORef False
  threeMarked  <- newIORef False
  fourMarked   <- newIORef False
  fiveMarked   <- newIORef False
  let vOne     =  Vertex { vid = 1, marked = oneMarked,   adj = [2, 3] }
      vTwo     =  Vertex { vid = 2, marked = twoMarked,   adj = [5]    }
      vThree   =  Vertex { vid = 3, marked = threeMarked, adj = [4]    }
      vFour    =  Vertex { vid = 4, marked = fourMarked,  adj = []     }
      vFive    =  Vertex { vid = 5, marked = fiveMarked,  adj = []     }
      g        =  Map.fromList [(1, vOne), (2, vTwo), (3, vThree), (4, vFour), (5, vFive)]
  bfsOrder    <- bfs g vid vOne
  resetGraph g
  dfsOrder    <- dfs g vid vOne
  putStr "BFS visit order: "
  print bfsOrder
  putStr "DFS visit order: "
  print dfsOrder
  
  
