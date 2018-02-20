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
  ( Graph,
    Vertex(..),
    neighbors,
    changeState,
    shouldVisit,
    resetGraph,
    buildGraph
  ) where

import           Data.IORef
import           Control.Monad
import           Data.Functor
import qualified Data.Map.Strict as Map

type Graph = Map.Map Int Vertex

-- | Representation of a vertex in an unweighed graph
data Vertex = Vertex
  { vid    :: Int
  , marked :: IORef Bool
  , adj    :: [Int]
  }

-- | Get all the neighbor vertices to
--   a given vertex.
neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = map (\vid -> g Map.! vid) (adj v)

-- | Change the marked state of a vertex
changeState :: Vertex -> IO ()
changeState v = modifyIORef (marked v) not

-- | Check wither the vertex is marked or not
shouldVisit :: Vertex -> IO Bool
shouldVisit v = do
  isMarked <- readIORef (marked v)
  return (not isMarked)

-- | Reset the state of all vertices of the graph
resetGraph :: Graph -> IO ()
resetGraph g = mapM_ (\(_, v) -> changeState v) (Map.toList g)

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
