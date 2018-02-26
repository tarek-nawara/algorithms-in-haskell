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
import           Data.IORef
import qualified Data.Map.Strict     as Map
import qualified Data.PQueue.Min     as MinPQ
import           GraphRepresentation

-- | Implementation of depth first algorithm
--   given the graph and a source vertex.
dfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
dfs g consumer source = do
  setMarked source True
  children <- filterM shouldVisit (neighbors g source)
  childrenRes <- mapM (dfs g consumer) children
  return $ consumer source : concat childrenRes

-- | Implementation of breadth first algorithm
--   given the graph and the source vertex.
bfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
bfs g consumer source = bfsInner g consumer [source]
  where
    bfsInner _ _ [] = return []
    bfsInner g consumer (source:rest) = do
      setMarked source True
      children <- filterM shouldVisit (neighbors g source)
      restRes <- bfsInner g consumer (rest ++ children)
      return $ consumer source : restRes

-- Check wither the vertex is marked or not
shouldVisit :: Vertex -> IO Bool
shouldVisit v = do
  isMarked <- readIORef (marked v)
  return (not isMarked)
