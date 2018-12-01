module EffectMonad where

import Prelude

import Effect (Effect)
import Effect.Random (random)
import Effect.Console (logShow)

main :: Effect Unit
main = logShow =<< pure <<< (_ * 100.0) =<< random