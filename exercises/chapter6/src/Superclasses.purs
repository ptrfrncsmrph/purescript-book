module Superclasses where

import Prelude
import Data.Foldable (maximum)
import Data.Maybe (fromJust)

findMax :: Partial => Array Int -> Int
findMax = fromJust <<< maximum

class Monoid m <= Action m a where
  act :: m -> a -> a

-- Action laws:
-- act mempty a     = a
-- act (m1 <> m2) a = act m1 (act m2 a)

ex :: Boolean
ex = (act (Multiply 2 <> Multiply 3) "poo") 
      == (act (Multiply 2) (act (Multiply 3) "poo"))

law1 :: forall m a. Eq a => (Action m (Array a)) => m -> Array a -> Boolean
law1 _ xs = act (mempty :: m) xs == xs

law2 :: forall m a. Eq a => (Action m (Array a)) => m -> m -> Array a -> Boolean
law2 m1 m2 xs = act (m1 <> m2) xs == act m1 (act m2 xs)

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance repeatAction :: Action Multiply String where
  act :: Multiply -> String -> String
  act (Multiply 1) s = s
  act (Multiply n) s = s <> act (Multiply (n - 1)) s

instance someAction :: Action m a => Action m (Array a) where
  act :: m -> Array a -> Array a
  act m = map (act m)

newtype Self m = Self m

instance selfAction :: Monoid m => Action m (Self m) where
  act :: m -> Self m -> Self m
  act x (Self x') = Self $ x <> x'