module AffMonad where

import Prelude
import Effect

-- asyncFunction :: forall success error
--                . (success -> Effect Unit)
--               -> (error -> Effect Unit)
--               -> Effect Unit
-- asyncFunction onSuccess onError =
--   ???