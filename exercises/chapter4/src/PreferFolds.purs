module PreferFolds where

import Prelude
import Data.Foldable (foldl)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

-- | 1.
allTrue :: Array Boolean -> Boolean
allTrue = foldl (==) true

-- | 2.
allFalse :: Array Boolean -> Boolean
allFalse = foldl (==) false

-- | 3.
count :: forall a. (a -> Boolean) -> Array a -> Int
count f = count' 0
  where
    count' acc [] = acc
    count' acc xs = count' (if f (unsafePartial head xs) == true then 1 + acc else acc)
                           (unsafePartial tail xs)
