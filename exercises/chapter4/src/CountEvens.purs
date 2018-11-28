module CountEvens where

import Prelude

import Data.Array (null)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Data.Int (even)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

countEvens :: Array Int -> Int
countEvens arr =
  if null arr
    then 0
    else (if even $ unsafePartial head arr
            then 1
            else 0) + countEvens (unsafePartial tail arr)