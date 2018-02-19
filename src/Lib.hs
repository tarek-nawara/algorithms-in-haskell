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

-- | Implementation of depth first algorithm
--   given the graph and a source vertex. 
dfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
dfs g consumer start = do
  modifyIORef (marked start) not
  children    <- filterM shouldVisit (neighbors g start)
  childrenRes <- mapM (dfs g consumer) children
  return $ consumer start : concat childrenRes
  where
    shouldVisit v = do
      isMarked <- readIORef (marked v)
      return (not isMarked)

-- TODO: this function should be removed
someFunc :: IO ()
someFunc = do
  sourceMarked <- newIORef False
  sinkMarked   <- newIORef False
  let source   =  Vertex { vid = 1, marked = sourceMarked, adj = [2] }
  let sink     =  Vertex { vid = 2, marked = sinkMarked,   adj = [1] }
  let g        =  Map.fromList [(1, source), (2, sink)]
  visitOrder   <- dfs g vid source
  print visitOrder
  
  
