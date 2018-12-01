module CombiningEffects where
  
import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))

maybePlus :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a 
maybePlus = lift2 (+)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing  = pure Nothing
combineMaybe (Just x) = Just <$> x