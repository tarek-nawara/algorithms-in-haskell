-------------------------------------
-- |
-- Module : GraphRepresentation
-- algorithms in haskell
-- Holder for the graph representation
-- and simple helper functions.
--
-- author tarek-nawara
--
---------------------------------------
module GraphRepresentation
  ( Graph
  , Vertex(..)
  , neighbors
  , setMarked
  , resetGraph
  , buildGraph
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Functor
import           Data.IORef
import qualified Data.Map.Strict as Map

type Graph = Map.Map Int Vertex

-- | Representation of a vertex
--   in an unweighed graph
data Vertex = Vertex
  { vid    :: Int
  , marked :: IORef Bool
  , adj    :: [Int]
  }

instance Show Vertex where
  show (Vertex vid _ adj) = "{id=" ++ show vid ++ ",adj=" ++ show adj

-- | Get all the neighbor vertices to
--   a given vertex.
neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = map (\vid -> g Map.! vid) (adj v)

-- | Change the marked state of an unweighted vertex
setMarked :: Vertex -> Bool -> IO ()
setMarked v newMarked = modifyIORef (marked v) (const newMarked)

-- | Reset the state of all vertices
--   in an unweighted graph.
resetGraph :: Graph -> IO ()
resetGraph g = mapM_ (\(_, v) -> setMarked v False) (Map.toList g)

-- | Build an unweighted graph from the adj list
buildGraph :: [[Int]] -> IO Graph
buildGraph l = Map.fromList <$> buildGraphInner 1 l
 where
  buildGraphInner _   []         = return []
  buildGraphInner vid (adj:rest) = do
    vMarked <- newIORef False
    let v = Vertex {vid = vid, marked = vMarked, adj = adj}
    restG <- buildGraphInner (vid + 1) rest
    return $ (vid, v) : restG
