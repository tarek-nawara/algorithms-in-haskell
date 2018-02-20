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
    Weight,
    WVertex(..),
    neighbors,
    changeState,
    changeWeight,
    shouldVisit,
    resetGraph,
    resetWGraph,
    buildGraph,
    buildWGraph
  ) where

import           Data.IORef
import           Control.Monad
import           Data.Functor
import qualified Data.Map.Strict as Map

type Graph  = Map.Map Int Vertex
type WGraph = Map.Map Int WVertex
type Edge   = (Int, Weight)

-- | Representation of any weight
--   in the graph.
data Weight = Infinity | Only Double

-- | Representation of a vertex
--   in an unweighed graph
data Vertex = Vertex
  { vid    :: Int
  , marked :: IORef Bool
  , adj    :: [Int]
  }

-- | Representation of a vertex in
--   a weighted graph.
data WVertex = WVertex
  { wvid    :: Int
  , weight  :: IORef Weight
  , outgo   :: [Edge]
  }

-- | Get all the neighbor vertices to
--   a given vertex.
neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = map (\vid -> g Map.! vid) (adj v)

-- | Change the marked state of a vertex
changeState :: Vertex -> IO ()
changeState v = modifyIORef (marked v) not

changeWeight :: WVertex -> Weight -> IO ()
changeWeight v newWeight = modifyIORef (weight v) (\_ -> newWeight)

-- | Check wither the vertex is marked or not
shouldVisit :: Vertex -> IO Bool
shouldVisit v = do
  isMarked <- readIORef (marked v)
  return (not isMarked)

-- | Reset the state of all vertices
--   in an unweighted graph.
resetGraph :: Graph -> IO ()
resetGraph g = mapM_ (\(_, v) -> changeState v) (Map.toList g)

-- | Reset the state of all vertices in
--   an weighted graph.
resetWGraph :: WGraph -> IO ()
resetWGraph g = mapM_ (\(_, v) -> changeWeight v Infinity) (Map.toList g)

-- | Build an unweighted graph from the adj list
buildGraph :: [[Int]] -> IO Graph
buildGraph l = Map.fromList <$> buildGraphInner 1 l
  where
    buildGraphInner _ [] = return []
    buildGraphInner vid (adj:rest) = do
      vMarked <- newIORef False
      let v =  Vertex { vid = vid, marked = vMarked, adj = adj }
      restG <- buildGraphInner (vid + 1) rest
      return $ (vid, v) : restG

-- | Build a weighted graph from adj list
buildWGraph :: [[Edge]] -> IO WGraph
buildWGraph l = Map.fromList <$> buildWGraphInner 1 l
  where
    buildWGraphInner _ [] = return []
    buildWGraphInner wvid (outgo:rest) = do
      vWeight <- newIORef Infinity
      let v = WVertex { wvid = wvid, weight = vWeight, outgo = outgo }
      restWG <- buildWGraphInner (wvid + 1) rest
      return $ (wvid, v) : restWG
