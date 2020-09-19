---------------------------------
-- |
-- Module : Main.hs
-- algorithms in haskell
-- Application entry point
--
-- author tarek-nawara
--
----------------------------------
module Main where

import qualified Data.Map.Strict      as Map
import BellmanFord

main :: IO ()
main = do
  let edges =
        [ Edge {src = 0, dest = 1, weight = 1}
        , Edge {src = 0, dest = 2, weight = 1}
        , Edge {src = 0, dest = 3, weight = 1}
        , Edge {src = 2, dest = 3, weight = 10}
        ]
  let res = bellmanFord 4 0 edges
  putStrLn (show res)
