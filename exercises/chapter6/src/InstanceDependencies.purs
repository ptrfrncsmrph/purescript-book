module InstanceDependencies where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldl, foldr)

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) 
     (NonEmpty y ys) = x == y && xs == ys

instance functorNonEmpty :: Functor (NonEmpty) where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

data Extended a = Finite a | Infinite

-- This Eq instance was not part of the exercise,
-- but in order to compile, it was needed for a
-- valid Ord instance. This makes it seem like
-- Eq is a superclass of Ord(?) and maybe it is...
-- ..._but_ maybe it's only required for higher-kinded
-- types? Hmm... maybe that doesn't make sense.
-- ***
-- On second thought, the first part is probably the case
-- i.e., I didn't need to explicitly _require_ an `Eq a`
-- because its implied by the `Ord a` instance
-- ***
-- Honestly haven't reached a conclusion
instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite x) (Finite y) = x == y
  eq Infinite Infinite     = true
  eq _ _                   = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite a) (Finite a') 
           | a > a'  = GT
           | a < a'  = LT
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare _ _        = EQ

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr :: forall a b. (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f b (NonEmpty x xs) = foldr f (f x b) xs
  foldl :: forall a b. (b -> a -> b) -> b -> NonEmpty a -> b
  foldl f b (NonEmpty x xs) = foldl f (f b x) xs
  foldMap :: forall a m. Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where  
  foldr :: forall a b. (a -> b -> b) -> b -> OneMore f a -> b
  foldr g b (OneMore a xs) = foldr g (g a b) xs
  foldl :: forall a b. (b -> a -> b) -> b -> OneMore f a -> b
  foldl g b (OneMore a xs) = foldl g (g b a) xs
  foldMap :: forall a m. Monoid m => (a -> m) -> OneMore f a -> m
  foldMap g (OneMore a xs) = g a <> foldMap g xs
