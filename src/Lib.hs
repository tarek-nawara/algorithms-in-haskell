module Lib
  ( someFunc
  ) where

import           Data.IORef
import           Control.Monad
import qualified Data.Map.Strict as Map

type Graph = Map.Map Int Vertex

data Vertex = Vertex
  { vid     :: Int
  , marked :: IORef Bool
  , adj    :: [Int]
  }

neighbors :: Graph -> Vertex -> [Vertex]
neighbors g v = map (\vid -> g Map.! vid) (adj v)

dfs :: Graph -> (Vertex -> a) -> Vertex -> IO [a]
dfs g consumer start = do
  modifyIORef (marked start) not
  children    <- filterM shouldVisit (neighbors g start)
  childrenRes <- (mapM (dfs g consumer) children)
  return $ (consumer start) : (concat childrenRes)
  where
    shouldVisit v = do
      isMarked <- readIORef (marked v)
      return (not isMarked)

someFunc :: IO ()
someFunc = do
  sourceMarked <- newIORef False
  sinkMarked   <- newIORef False
  let source   =  Vertex { vid = 1, marked = sourceMarked, adj = [2] }
  let sink     =  Vertex { vid = 2, marked = sinkMarked,   adj = [1] }
  let g        =  Map.fromList [(1, source), (2, sink)]
  visitOrder   <- dfs g vid source
  print visitOrder
  
  
