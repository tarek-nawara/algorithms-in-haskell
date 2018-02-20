-------------------------------------
-- |
-- Module : GraphAlgorithms
-- algorithms in haskell
-- Implementation of graph algorithms
-- in haskell.
-- 
-- author tarek-nawara
--
---------------------------------------
module GraphAlgorithms
  ( dfs,
    bfs
  ) where

import           Data.IORef
import           Control.Monad
import           Data.Functor
import           GraphRepresentation
import qualified Data.Map.Strict as Map
  
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
