module Test where

import           Control.Monad (guard)

factors :: Int -> [[Int]]
factors n = do
  i <- [1 .. n]
  j <- [i .. n]
  guard $ i * j == n
  pure [i, j]

factorizations :: Int -> [[Int]]
factorizations n = [[n]] ++ go [] (factors n)
  where
    go :: [Int] -> [[Int]] -> [[Int]]
    go acc (xs:xss) = undefined
