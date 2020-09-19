-------------------------------------------
-- |
-- Module : WGraphRepresentation
-- algorithms in haskell
-- Holder for weighted graph representation
-- and simple helper functions.
--
-- author tarek-nawara
--
--------------------------------------------
module WGraphRepresentation
  ( WGraph
  , Weight(..)
  , WVertex(..)
  , wneighbors
  , setWMarked
  , setWeight
  , resetWGraph
  , buildWGraph
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Functor
import           Data.IORef
import qualified Data.Map.Strict as Map

type WGraph = Map.Map Int WVertex

type Edge = (Int, Double)

-- | Representation of any weight
--   in the graph.
data Weight
  = Infinity
  | Only Double
  deriving (Show, Eq)

-- | Representation of a vertex in
--   a weighted graph.
data WVertex = WVertex
  { wvid    :: Int
  , wmarked :: IORef Bool
  , weight  :: IORef Weight
  , outgo   :: [Edge]
  }

instance Show WVertex where
  show (WVertex wvid _ _ outgo) = "{id=" ++ show wvid ++ ",outgo=" ++ show outgo

wneighbors :: WGraph -> WVertex -> [(WVertex, Double)]
wneighbors g v = map (first ((Map.!) g)) (outgo v)

-- | Change the marked state of weighed vertex
setWMarked :: WVertex -> Bool -> IO ()
setWMarked v newMarked = modifyIORef (wmarked v) (const newMarked)

-- | Change the weight of a vertex
--   in a weighted graph.
setWeight :: WVertex -> Weight -> IO ()
setWeight v newWeight = modifyIORef (weight v) (const newWeight)

-- | Reset the state of all vertices in
--   an weighted graph.
resetWGraph :: WGraph -> IO ()
resetWGraph g = mapM_ (\(_, v) -> resetVertex v) (Map.toList g)
 where
  resetVertex v = do
    setWMarked v False
    setWeight  v Infinity

-- | Build a weighted graph from adj list
buildWGraph :: [[Edge]] -> IO WGraph
buildWGraph l = Map.fromList <$> buildWGraphInner 1 l
 where
  buildWGraphInner _    []           = return []
  buildWGraphInner wvid (outgo:rest) = do
    vWeight <- newIORef Infinity
    vMarked <- newIORef False
    let
      v = WVertex
        { wvid    = wvid
        , wmarked = vMarked
        , weight  = vWeight
        , outgo   = outgo
        }
    restWG <- buildWGraphInner (wvid + 1) rest
    return $ (wvid, v) : restWG
