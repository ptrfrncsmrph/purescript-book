module MonadError where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)

data UserInfo = UserInfo
data User = User
data Item = Item
data Receipt = Receipt
data Basket = Basket

registerUser :: UserInfo -> Aff (Either Error User)
registerUser user = pure $ Right User

createBasket :: User -> Aff (Either Error Basket)
createBasket user = pure $ Right Basket

addItemToBasket :: Item -> Basket -> Aff (Either Error Basket)
addItemToBasket item basket = pure $ Right basket

purchaseBasket :: User -> Basket -> Aff (Either Error Receipt)
purchaseBasket user basket = pure $ Right Receipt

quickCheckout :: Item -> UserInfo -> Aff (Either Error Receipt)
quickCheckout item userInfo = do
  eitherRegister <- registerUser userInfo
  case eitherRegister of
    Left error -> pure $ Left error
    Right user -> do
      eitherBasket <- createBasket user
      case eitherBasket of
        Left error -> pure $ Left error
        Right basket -> do
          eitherItemInBasket <- addItemToBasket item basket
          case eitherItemInBasket of
            Left error -> pure $ Left error
            Right itemInBasket -> purchaseBasket user itemInBasket

rethrow :: forall a. Aff (Either Error a) -> Aff a
rethrow aff = do
  either <- aff
  case either of
    Left error -> throwError error
    Right a -> pure a

quickCheckout' :: Item -> UserInfo -> Aff Receipt
quickCheckout' item userInfo = do
  user <- registerUser userInfo # rethrow
  basket <- createBasket user # rethrow
  itemInBasket <- addItemToBasket item basket # rethrow
  purchaseBasket user itemInBasket # rethrow

main :: Effect Unit
main = launchAff_ do
  either <- try $ quickCheckout' Item UserInfo
  case either of
    Left error -> log "There was an error checking out!" # liftEffect
    Right _ -> log "Checkout successful!" # liftEffect