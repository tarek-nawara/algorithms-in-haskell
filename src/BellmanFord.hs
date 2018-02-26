---------------------------------------------
-- |
-- Module : BellmanFord
-- algorithms in haskell
-- Implementation of bellman ford algorithms
-- in haskell.
--
-- author tarek-nawara
--
---------------------------------------------
module BellmanFord
  ( bellmanFord
  , Edge(..)
  ) where

import           Control.Monad
import           Data.IORef
import qualified Data.Map.Strict      as Map
import           WGraphRepresentation

-- | Representation of a weighted edge
data Edge = Edge
  { src        :: Int
  , dest       :: Int
  , edgeWeight :: Double
  }

-- | Implementation of bellman ford graph
--   algorithm to relax all vertices
--   given source vertex.
bellmanFord :: Int -> WGraph -> [Edge] -> IO (Map.Map Int Weight)
bellmanFord source g edges = do
  let srcVertex = g Map.! source
  setWeight srcVertex (Only 0)
  forM_ (Map.keys g) $ \_ -> forM_ edges $ \edge -> relaxWVertex g edge
  getAllVerticesWeight g

-- Relax dest vertex.
relaxWVertex :: WGraph -> Edge -> IO ()
relaxWVertex g edge = do
  let x = g Map.! src edge
  let y = g Map.! dest edge
  xwRef <- readIORef (weight x)
  ywRef <- readIORef (weight y)
  case (xwRef, ywRef) of
    (Infinity, _) -> return ()
    (Only xw, Infinity) -> setWeight y (Only $ edgeWeight edge + xw)
    (Only xw, Only yw) -> do
      let newWeight = edgeWeight edge + xw
      when (newWeight < yw) $ setWeight y (Only newWeight)

-- Build bellman ford final result
getAllVerticesWeight :: WGraph -> IO (Map.Map Int Weight)
getAllVerticesWeight g = do
  l <- mapM mapper (Map.toList g)
  return $ Map.fromList l
  where
    mapper (k, v) = do
      w <- readIORef (weight v)
      return (k, w)
