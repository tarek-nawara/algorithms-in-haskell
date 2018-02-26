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
  ( dfs
  , bfs
  , dijkstra
  ) where

import           Control.Monad
import           Data.IORef
import qualified Data.Map.Strict     as Map
import qualified Data.PQueue.Min     as MinPQ
import           GraphRepresentation

-- | Representation of the data that we
--   add in the priority queue.
data PQState = PQState
  { stateid     :: Int
  , stateWeight :: Double
  }

instance Eq PQState where
  (PQState idOne wOne) == (PQState idTwo wTwo) =
    (idOne == idTwo) && (wOne == wTwo)

instance Ord PQState where
  (PQState idOne wOne) `compare` (PQState idTwo wTwo) =
    case wcmp of
      EQ -> idOne `compare` idTwo
      _  -> wcmp
    where
      wcmp = wOne `compare` wTwo

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

-- | Implementation of Dijkstra algorithm
--   this implementation will return all shortest path
--   between the given source node and all
--   the graph nodes.
dijkstra :: WGraph -> WVertex -> IO (Map.Map Int Weight)
dijkstra g source = do
  setWeight source (Only 0)
  startState <- vertexToPQState source
  resMap <- dijkstraImpl g (MinPQ.singleton startState) Map.empty
  let nonIncludedKeys =
        Map.fromList [(k, Infinity) | k <- Map.keys g, Map.notMember k resMap]
  return $ resMap `Map.union` nonIncludedKeys

-- check if the node is already marked
-- as part of the dijkstra graph
shouldTestForRelax :: WVertex -> IO Bool
shouldTestForRelax v = do
  isMarked <- readIORef (wmarked v)
  return (not isMarked)

-- compare the current weight of the vertex
-- with the new weight it can have.
shouldRelax :: Weight -> Double -> Bool
shouldRelax Infinity _                 = True
shouldRelax (Only curWeight) newWeight = curWeight > newWeight

-- changing the weight of a vertex with the
-- given new weight
relaxChild :: Double -> WVertex -> IO (Maybe WVertex)
relaxChild newWeight v = do
  vertexWeight <- readIORef (weight v)
  if shouldRelax vertexWeight newWeight
    then do
      setWeight v (Only newWeight)
      return (Just v)
    else return Nothing

-- relax all the children of a given vertex
-- return only the relaxed vertices to add them
-- in the pq.
relaxChildren :: Double -> [(WVertex, Double)] -> IO [WVertex]
relaxChildren _ [] = return []
relaxChildren parentWeight ((v, edgeWeight):rest) = do
  restRes <- relaxChildren parentWeight rest
  let newWeight = parentWeight + edgeWeight
  curVertexRes <- relaxChild newWeight v
  case curVertexRes of
    Nothing       -> return restRes
    Just relaxedV -> return $ relaxedV : restRes

-- convert a vertex to a state to add it
-- in the priority queue
vertexToPQState :: WVertex -> IO PQState
vertexToPQState v = do
  (Only vertexWeight) <- readIORef (weight v)
  return PQState {stateid = wvid v, stateWeight = vertexWeight}

-- Actual implementation of dijkstra algorithm
dijkstraImpl ::
     WGraph
  -> MinPQ.MinQueue PQState
  -> Map.Map Int Weight
  -> IO (Map.Map Int Weight)
dijkstraImpl g pq acc =
  if MinPQ.null pq
    then return acc
    else do
      let (curState, newPQ) = MinPQ.deleteFindMin pq
      let (curId, parentWeight) = (stateid curState, stateWeight curState)
      let curVertex = g Map.! curId
      isMarked <- readIORef (wmarked curVertex)
      if isMarked
        then dijkstraImpl g newPQ acc
        else do
          setWMarked curVertex True
          let newAcc = Map.insert curId (Only parentWeight) acc
          children <-
            filterM (\(v, _) -> shouldTestForRelax v) (wneighbors g curVertex)
          relaxedChildren <- relaxChildren parentWeight children
          newStates <- mapM vertexToPQState relaxedChildren
          let childPQ = MinPQ.fromList newStates
          dijkstraImpl g (newPQ `MinPQ.union` childPQ) newAcc
