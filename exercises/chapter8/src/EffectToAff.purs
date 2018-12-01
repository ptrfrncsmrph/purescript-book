module EffectToAff where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Random (random)

printRandomStyle1a :: Aff Unit
printRandomStyle1a = liftEffect doRandom
  where
    doRandom :: Effect Unit
    doRandom = do
      n <- random
      logShow n

printRandomStyle1b :: Aff Unit
printRandomStyle1b = liftEffect $ do
  n <- random
  logShow n

printRandomStyle2 :: Aff Unit
printRandomStyle2 = do
  n <- liftEffect random
  liftEffect $ logShow n

printRandomStyle3 :: Aff Unit
printRandomStyle3 = do
  n <- random # liftEffect
  (logShow n) # liftEffect

main :: Effect Unit
main = launchAff_ do
  printRandomStyle1a
  printRandomStyle1b
  printRandomStyle2
  printRandomStyle3