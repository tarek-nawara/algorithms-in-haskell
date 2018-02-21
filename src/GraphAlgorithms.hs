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
    bfs,
    dijkstra,
  ) where

import           Data.IORef
import           Control.Monad
import           GraphRepresentation
import qualified Data.PQueue.Min as MinPQ
import qualified Data.Map.Strict as Map

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
      _ -> wcmp
    where
      wcmp = wOne `compare` wTwo

  
-- Check wither the vertex is marked or not
shouldVisit :: Vertex -> IO Bool
shouldVisit v = do
  isMarked <- readIORef (marked v)
  return (not isMarked)

-- | Implementation of depth first algorithm
--   given the graph and a source vertex. 
dfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
dfs g consumer source = do
  setMarked source True
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
      setMarked source True
      children <- filterM shouldVisit (neighbors g source)
      restRes  <- bfsInner g consumer (rest ++ children)
      return $ consumer source : restRes

dijkstra :: WGraph -> WVertex -> IO (Map.Map Int Weight)
dijkstra g source =
  dijkstraImpl g startPQ Map.empty
  where
    startPQ = MinPQ.singleton PQState { stateid = wvid source, stateWeight = 0 }


shouldTestForRelax :: WVertex -> IO Bool
shouldTestForRelax v = do
  isMarked <- readIORef (wmarked v)
  return (not isMarked)

shouldRelax :: Weight -> Double -> Bool
shouldRelax Infinity _ = True
shouldRelax (Only curWeight) newWeight = curWeight > newWeight

relaxChild :: Double -> Double -> WVertex -> IO (Maybe WVertex)
relaxChild parentWeight edgeWeight v = do
  vertexWeight <- readIORef (weight v)
  let newWeight = parentWeight + edgeWeight
  if shouldRelax vertexWeight newWeight
    then do
    setWeight v (Only newWeight)
    return (Just v)
    else return Nothing

relaxChildren :: Double -> [(WVertex, Double)] -> IO [WVertex]
relaxChildren _ [] = return []
relaxChildren parentWeight ((v, edgeWeight):rest) = do
  restRes <- relaxChildren parentWeight rest
  curVertexRes <- relaxChild parentWeight edgeWeight v
  case curVertexRes of
    Nothing -> return restRes
    Just relaxedV -> return $ relaxedV : restRes

vertexToPQState :: WVertex -> IO PQState
vertexToPQState v = do
  (Only vertexWeight) <- readIORef (weight v)
  return PQState { stateid = wvid v, stateWeight = vertexWeight }

-- TODO should check if the state we pull from
-- the pq is visited or not.
dijkstraImpl
  :: WGraph
  -> MinPQ.MinQueue PQState
  -> Map.Map Int Weight
  -> IO (Map.Map Int Weight)
dijkstraImpl g pq acc =
  if MinPQ.null pq
    then return acc
    else do
    let (curState, newPQ) = MinPQ.deleteFindMin pq
    let (curId, parentWeight) = (stateid curState, stateWeight curState)
    let newAcc = Map.insert curId (Only parentWeight) acc
    let curVertex = g Map.! curId
    setWMarked curVertex True
    children <- filterM (\ (v, _) -> shouldTestForRelax v) (wneighbors g curVertex)
    relaxedChildren <- relaxChildren parentWeight children
    newStates <- mapM vertexToPQState relaxedChildren
    let childPQ = MinPQ.fromList newStates
    dijkstraImpl g (newPQ `MinPQ.union` childPQ) newAcc
        
