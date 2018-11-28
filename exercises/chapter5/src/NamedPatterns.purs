module NamedPatterns where

import Prelude

type Person =
  { name    :: String
  , address :: Address 
  }

type Address =
  { street :: String
  , city   :: String
  }

sameCity :: 
  forall r0 r1 r2 r3 c.
    Eq c
    => { address :: { city :: c | r0 } | r1 } 
    -> { address :: { city :: c | r2 } | r3 } 
    -> Boolean
sameCity { address: { city: a } } { address: { city: b } } = a == b

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _   = x