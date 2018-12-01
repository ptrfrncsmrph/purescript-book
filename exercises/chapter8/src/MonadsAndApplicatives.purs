module MonadsAndApplicatives where

import Prelude

import Data.Array (head, tail, sort, nubBy)
import Data.Maybe (Maybe)
import Data.List (List(..), foldM, fromFoldable, (:))
import Data.Traversable (sequence, foldr)

third :: forall a. Array a -> Maybe a
third xs = do
  xs' <- tail xs
  xs'' <- tail xs'
  head xs''

third' :: forall a. Array a -> Maybe a
third' xs = tail xs >>= tail >>= head

third'' :: forall a. Array a -> Maybe a
third'' = (head =<< _) <<< (tail =<< _) <<< tail

third''' :: forall a. Array a -> Maybe a
third''' = head <=< tail <=< tail

sums :: Array Int -> Array Int
sums xs = sort $ nubBy compare $ foldM f 0 (fromFoldable xs)
  where
    f :: (Int -> Int -> Array Int)
    f n m = do
      x <- [0, n]
      y <- [0, m]
      pure $ x + y
  

filterM :: forall m a
         . Monad m
        => (a -> m Boolean)
        -> List a            
        -> m (List a)
-- | This is gross :<
filterM p xs = foldr foo (pure Nil) xs
  where
    foo :: a -> m (List a) -> m (List a)
    foo x mxs = 
      goo 
      <$> (p x) 
      <*> mxs
        where
          goo true  = (x : _) 
          goo false = identity

bar :: Int -> Array Boolean
bar = sequence [isOdd, isEven, isZero]
  where
    isOdd y  = y `mod` 2 == 1
    isEven y = y `mod` 2 == 0
    isZero y = y == 0
