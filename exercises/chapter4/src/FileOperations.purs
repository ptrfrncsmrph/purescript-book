module FileOperations where

import Prelude

import Data.Array (concatMap, (:), filter)
import Data.Array as Array
import Data.Path (Path, isDirectory, ls, size)
import Data.Maybe (Maybe(..), fromJust)
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)
import Data.Array.Partial (head, tail)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

minAndMax :: Path -> Array Path
minAndMax p = [minimum', maximum']
  where
    minimum' :: Path
    minimum' = foldl keepMin p' ps' 
    keepMin f1 f2 = 
      if (size' f1) > (size' f2)
        then f2 
        else f1
    maximum' :: Path
    maximum' = foldl keepMax p' ps'
    keepMax f1 f2 = 
      if (size' f1) > (size' f2)
        then f1 
        else f2
    size' = unsafePartial $ fromJust <<< size 
    p' = unsafePartial $ head $ onlyFiles p
    ps' = unsafePartial $ tail $ onlyFiles p

whereIs :: String -> Path -> Maybe Path
whereIs q p = case (filter ((_ == q) <<< show) $ allFiles p) of
  [] -> Nothing
  x -> Array.head x

-- whereIs' :: String -> Path -> Maybe Path
