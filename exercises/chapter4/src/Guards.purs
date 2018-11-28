module Guards where

import Prelude

import Data.Array ((..), (:), filter, concat, concatMap)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Data.Semigroup ((<>))
import Data.Foldable (product)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

triples :: Int -> Array (Array Int) 
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k 
  pure [i, j, k]

-- factorizations :: Int -> Array (Array Int)
-- factorizations n = 
--   [[n]] <> go [] (factors n)
--      where go :: Array Int -> Array (Array Int) -> Array (Array Int)
--            go acc (xs:xss) = undefined

-- factorizations' :: Array Int -> Int -> Array (Array Int)
-- factorizations' _ 1 = [[]]
-- factorizations' [x] _ = [[x]]
-- factorizations' xs n = do
--   ts <- tails xs
--   rest <- factorizations' ts (n - 1)
--   [(unsafePartial $ head ts) : rest]
--     where tails :: Array Int -> Array (Array Int)
--           tails [x] = [[x]]
--           tails ys = (unsafePartial $ tail ys) : tails ys  

-- factorizations :: Int -> Array (Array Int)
-- factorizations n = filter (\xs -> product xs == n) $ factorizations' (1 .. n) n

-- factorizations :: Int -> Array (Array Int)
-- factorizations 2 = [[2]]
-- factorizations n =
--   [[n]] <> (factors' n)
--     where
--       factors' m = do
--         i <- 2 .. m
--         j <- i .. m
--         guard $ i * j == m
--         [[i, j]] <> factorizations j



  -- k <- factors j
  -- pure $ concat [[i,j], [i] <> k]

-- factorizations :: Int -> Array (Array Int)
-- factorizations 1 = [[1]]
-- factorizations n = do
--   i <- 1 .. n
--   j <- i .. n
--   guard $ i * j == n
--   xs <- unsafePartial tail [i, j]
--   concat [[[i, j]], factorizations xs]

-- factorizations :: Int -> Array (Array Int)
-- factorizations n = draw (1 .. n) n

-- draw :: Array Int -> Int -> Array (Array Int)
-- draw _ 0  = [[]]
-- draw xs n = do
--   ts <- tails xs
--   rest <- draw ts (n - 1)
--   pure [unsafePartial head ts : rest]
--     where
--       -- tails 
--       tails [] = []
--       tails xs = [tail xs] : (tails xs)

-- [t : rest | ts@(t:_) <- tails xs, rest <- draw ts (n - 1)]
