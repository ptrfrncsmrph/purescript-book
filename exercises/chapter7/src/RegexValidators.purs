module RegexValidators where

import Prelude
import Data.AddressBook.Validation (Errors, lengthIs, matches, nonEmpty)
import Data.AddressBook (Address(..), address)
import Data.Either (Either(..))
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V)
import Partial.Unsafe (unsafePartial)

stateLettersRegex :: Regex
stateLettersRegex = 
  unsafePartial
    case regex "[A-Z]{2}" noFlags of
      Right r -> r

validateState :: String -> V Errors String
validateState s = matches "State" stateLettersRegex s *> pure s

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> 
               validateState o.state      *> pure o.state)