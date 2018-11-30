module Complex where

import Prelude

newtype Complex = Complex
    { real ::      Number
    , imaginary :: Number
    }

instance showComplex :: Show Complex where
  -- show (Complex { real, imaginary }) = 
  --   "Complex { real: " 
  --   <> show real
  --   <> ", imaginary: "
  --   <> show imaginary
  --   <> " }"
  show (Complex { real, imaginary }) =
    show real <> " + " <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
  -- eq (Complex { real, imaginary }) 
  --    (Complex { real, imaginary }) = true
  -- eq _ _                           = false
  eq (Complex { real: r0, imaginary: i0 }) 
     (Complex { real: r1, imaginary: i1 })
       = r0 == r1 && i0 == i1