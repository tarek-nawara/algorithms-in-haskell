---------------------------------------------
-- |
-- Module : BellmanFord
-- algorithms in haskell
-- Implementation of bellman ford algorithms
-- in haskell.
--
-- author tknawara
--
---------------------------------------------
module BellmanFord
  ( bellmanFord
  , Edge(..)
  , Weight(..)
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as Map

data Weight
  = Infinity
  | Only Double
  deriving (Show, Eq)

newtype BState =
  BState (Map.Map Int Weight)

-- | Representation of a weighted edge
data Edge =
  Edge
    { src :: Int
    , dest :: Int
    , weight :: Double
    }

-- | Implementation of bellman ford graph
--   algorithm to relax all vertices
--   given source vertex.
bellmanFord :: Int -> Int -> [Edge] -> Map.Map Int Weight
bellmanFord n source edges =
  let initialWeights = Map.fromList [(i, Infinity) | i <- [0 .. (n - 1)]]
      initialState = BState (Map.insert source (Only 0) initialWeights)
      (BState weights) =
        foldl
          (\state _ -> execState (relaxAll edges) state)
          initialState
          [1 .. n]
   in weights
  where
    relaxAll edges = mapM_ relax edges

-- relax the dest vertex of the
-- given edge, using the current state
-- which has the all the weights
relax :: Edge -> State BState ()
relax edge = do
  (BState weights) <- get
  let srcWeight = weights Map.! src edge
  let destWeight = weights Map.! dest edge
  let edgeWeight = weight edge
  case (srcWeight, destWeight) of
    (Infinity, _) -> return ()
    (Only sw, Infinity) -> update (dest edge) (sw + edgeWeight)
    (Only sw, Only dw) ->
      when (dw > sw + edgeWeight) $ update (dest edge) (sw + edgeWeight)
  where
    update :: Int -> Double -> State BState ()
    update vertex weight = do
      (BState weights) <- get
      let newWeights = BState (Map.insert vertex (Only weight) weights)
      put newWeights
      return ()
