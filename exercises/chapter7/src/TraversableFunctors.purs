module TraversableFunctors where
  
import Prelude
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance functorTree :: Functor Tree

instance foldableTree :: Foldable Tree where
  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _ b Leaf = b
  foldr f b (Branch xs x ys) = foldr f (foldr f (f x b) ys) xs
  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ b Leaf = b
  foldl f b (Branch xs x ys) = foldl f (foldl f (f b x) ys) xs
  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch xs x ys) = foldMap f xs <> f x <> foldMap f ys 

instance traversableTree :: Traversable Tree where
  sequence :: forall a m.
              Applicative m
           => Tree (m a)
           -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch xs x ys) = Branch <$> 
                              sequence xs <*> 
                              x <*> 
                              sequence ys
  traverse :: forall a b m. 
              Applicative m 
           => (a -> m b)
           -> Tree a
           -> m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch xs x ys) = Branch <$> 
                                traverse f xs <*> 
                                f x <*> 
                                traverse f ys
  -- traverse f xs = map f $ sequence xs

