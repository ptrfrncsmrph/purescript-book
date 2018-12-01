# The Effect and Aff Monads

## Chapter Goals

In the last chapter, we introduced applicative functors, an abstraction which we used to deal with _side-effects_: optional values, error messages and validation. This chapter will introduce another abstraction for dealing with side-effects in a more expressive way: _monads_.

The goal of this chapter is to explain why monads are a useful abstraction, and their connection with _do notation_. <!-- We will build upon the address book example of the previous chapters, by using a particular monad to handle the side-effects of building a user interface in the browser. The monad we will use is an important monad in PureScript - the `Effect` monad - used to encapsulate so-called _native_ effects. -->
TODO: describe how Effect and Aff will be used in examples

## Project Setup
TODO: describe the project setup
<!--
The source code for this project builds on the source for the previous chapter. The modules from the previous project are included in the `src` directory for this project.

The project adds the following psc-package dependencies:

- `purescript-effect`, which defines the `Effect` monad, the subject of the second half of the chapter.
- `purescript-aff`, a set of bindings to the React user interface library, which we will use to build a user interface for our address book application.

In addition to the modules from the previous chapter, this chapter's project adds a `Main` module, which provides the entry point to the application, and functions to render the user interface.

To compile this project, first install React using `npm install`, and then build and bundle the JavaScript source with `pulp browserify --to dist/Main.js`. To run the project, open the `html/index.html` file in your web browser.
-->
## Monads and Do Notation

Do notation was first introduced when we covered _array comprehensions_. Array comprehensions provide syntactic sugar for the `concatMap` function from the `Data.Array` module.

Consider the following example. Suppose we throw two dice and want to count the number of ways in which we can score a total of `n`. We could do this using the following non-deterministic algorithm:

- _Choose_ the value `x` of the first throw.
- _Choose_ the value `y` of the second throw.
- If the sum of `x` and `y` is `n` then return the pair `[x, y]`, else fail.

Array comprehensions allow us to write this non-deterministic algorithm in a natural way:

```haskell
import Prelude

import Control.Plus (empty)
import Data.Array ((..))

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [x, y]
    else empty
```

We can see that this function works in PSCi:

```text
> countThrows 10
[[4,6],[5,5],[6,4]]

> countThrows 12  
[[6,6]]
```

In the last chapter, we formed an intuition for the `Maybe` applicative functor, embedding PureScript functions into a larger programming language supporting _optional values_. In the same way, we can form an intuition for the _array monad_, embedding PureScript functions into a larger programming language supporting _non-deterministic choice_.

In general, a _monad_ for some type constructor `m` provides a way to use do notation with values of type `m a`. Note that in the array comprehension above, every line contains a computation of type `Array a` for some type `a`. In general, every line of a do notation block will contain a computation of type `m a` for some type `a` and our monad `m`. The monad `m` must be the same on every line (i.e. we fix the side-effect), but the types `a` can differ (i.e. individual computations can have different result types).

Here is another example of do notation, this type applied to the type constructor `Maybe`. Suppose we have some type `XML` representing XML nodes, and a function

```haskell
child :: XML -> String -> Maybe XML
```

which looks for a child element of a node, and returns `Nothing` if no such element exists.

In this case, we can look for a deeply-nested element by using do notation. Suppose we wanted to read a user's city from a user profile which had been encoded as an XML document:

```haskell
userCity :: XML -> Maybe XML
userCity root = do
  prof <- child root "profile"
  addr <- child prof "address"
  city <- child addr "city"
  pure city
```

The `userCity` function looks for a child element `profile`, an element `address` inside the `profile` element, and finally an element `city` inside the `address` element. If any of these elements are missing, the return value will be `Nothing`. Otherwise, the return value is constructed using `Just` from the `city` node.

Remember, the `pure` function in the last line is defined for every `Applicative` functor. Since `pure` is defined as `Just` for the `Maybe` applicative functor, it would be equally valid to change the last line to `Just city`.

## The Monad Type Class

The `Monad` type class is defined as follows:

```haskell
class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m
```

The key function here is `bind`, defined in the `Bind` type class. Just like for the `<$>` and `<*>` operators in the `Functor` and `Apply` type classes, the Prelude defines an infix alias `>>=` for the `bind` function.

The `Monad` type class extends `Bind` with the operations of the `Applicative` type class that we have already seen.

It will be useful to see some examples of the `Bind` type class. A sensible definition for `Bind` on arrays can be given as follows:

```haskell
instance bindArray :: Bind Array where
  bind xs f = concatMap f xs
```

This explains the connection between array comprehensions and the `concatMap` function that has been alluded to before.

Here is an implementation of `Bind` for the `Maybe` type constructor:

```haskell
instance bindMaybe :: Bind Maybe where
  bind Nothing  _ = Nothing
  bind (Just a) f = f a
```

This definition confirms the intuition that missing values are propagated through a do notation block.

Let's see how the `Bind` type class is related to do notation. Consider a simple do notation block which starts by binding a value from the result of some computation:

```haskell
do value <- someComputation
   whatToDoNext
```

Every time the PureScript compiler sees this pattern, it replaces the code with this:

```haskell
bind someComputation \value -> whatToDoNext
```

or, written infix:

```haskell
someComputation >>= \value -> whatToDoNext
```

The computation `whatToDoNext` is allowed to depend on `value`.

If there are multiple binds involved, this rule is applied multiple times, starting from the top. For example, the `userCity` example that we saw earlier gets desugared as follows:

```haskell
userCity :: XML -> Maybe XML
userCity root =
  child root "profile" >>= \prof ->
    child prof "address" >>= \addr ->
      child addr "city" >>= \city ->
        pure city
```

It is worth noting that code expressed using do notation is often much clearer than the equivalent code using the `>>=` operator. However, writing binds explicitly using `>>=` can often lead to opportunities to write code in _point-free_ form - but the usual warnings about readability apply.

## Monad Laws

The `Monad` type class comes equipped with three laws, called the _monad laws_. These tell us what we can expect from sensible implementations of the `Monad` type class.

It is simplest to explain these laws using do notation.

### Identity Laws

The _right-identity_ law is the simplest of the three laws. It tells us that we can eliminate a call to `pure` if it is the last expression in a do notation block:

```haskell
do
  x <- expr
  pure x
```

The right-identity law says that this is equivalent to just `expr`.

The _left-identity_ law states that we can eliminate a call to `pure` if it is the first expression in a do notation block:

```haskell
do
  x <- pure y
  next
```

This code is equivalent to `next`, after the name `x` has been replaced with the expression `y`.

The last law is the _associativity law_. It tells us how to deal with nested do notation blocks. It states that the following piece of code:

```haskell
c1 = do
  y <- do
    x <- m1
    m2
  m3
```

is equivalent to this code:

```haskell  
c2 = do
  x <- m1
  y <- m2
  m3
```

Each of these computations involves three monadic expression `m1`, `m2` and `m3`. In each case, the result of `m1` is eventually bound to the name `x`, and the result of `m2` is bound to the name `y`.

In `c1`, the two expressions `m1` and `m2` are grouped into their own do notation block.

In `c2`, all three expressions `m1`, `m2` and `m3` appear in the same do notation block.

The associativity law tells us that it is safe to simplify nested do notation blocks in this way.

_Note_ that by the definition of how do notation gets desugared into calls to `bind`, both of `c1` and `c2` are also equivalent to this code:

```haskell  
c3 = do
  x <- m1
  do
    y <- m2
    m3
```

## Folding With Monads

As an example of working with monads abstractly, this section will present a function which works with any type constructor in the `Monad` type class. This should serve to solidify the intuition that monadic code corresponds to programming "in a larger language" with side-effects, and also illustrate the generality which programming with monads brings.

The function we will write is called `foldM`. It generalizes the `foldl` function that we met earlier to a monadic context. Here is its type signature:

```haskell
foldM :: forall m a b
       . Monad m
      => (a -> b -> m a)
      -> a
      -> List b
      -> m a
```

Notice that this is the same as the type of `foldl`, except for the appearance of the monad `m`:

```haskell
foldl :: forall a b
       . (a -> b -> a)
      -> a
      -> List b
      -> a
```

Intuitively, `foldM` performs a fold over a list in some context supporting some set of side-effects.

For example, if we picked `m` to be `Maybe`, then our fold would be allowed to fail by returning `Nothing` at any stage - every step returns an optional result, and the result of the fold is therefore also optional.

If we picked `m` to be the `Array` type constructor, then every step of the fold would be allowed to return zero or more results, and the fold would proceed to the next step independently for each result. At the end, the set of results would consist of all folds over all possible paths. This corresponds to a traversal of a graph!

To write `foldM`, we can simply break the input list into cases.

If the list is empty, then to produce the result of type `a`, we only have one option: we have to return the second argument:

```haskell
foldM _ a Nil = pure a
```

Note that we have to use `pure` to lift `a` into the monad `m`.

What if the list is non-empty? In that case, we have a value of type `a`, a value of type `b`, and a function of type `a -> b -> m a`. If we apply the function, we obtain a monadic result of type `m a`. We can bind the result of this computation with a backwards arrow `<-`.

It only remains to recurse on the tail of the list. The implementation is simple:

```haskell
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs
```

Note that this implementation is almost identical to that of `foldl` on lists, with the exception of do notation.

We can define and test this function in PSCi. Here is an example - suppose we defined a "safe division" function on integers, which tested for division by zero and used the `Maybe` type constructor to indicate failure:

```haskell
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)
```

Then we can use `foldM` to express iterated safe division:  

```text
> import Data.List

> foldM safeDivide 100 (fromFoldable [5, 2, 2])
(Just 5)

> foldM safeDivide 100 (fromFoldable [2, 0, 4])
Nothing
```

The `foldM safeDivide` function returns `Nothing` if a division by zero was attempted at any point. Otherwise it returns the result of repeatedly dividing the accumulator, wrapped in the `Just` constructor.

## Monads and Applicatives

Every instance of the `Monad` type class is also an instance of the `Applicative` type class, by virtue of the superclass relationship between the two classes.

However, there is also an implementation of the `Applicative` type class which comes "for free" for any instance of `Monad`, given by the `ap` function:

```haskell
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  pure (f a)
```

If `m` is a law-abiding member of the `Monad` type class, then there is a valid `Applicative` instance for `m` given by `ap`.

The interested reader can check that `ap` agrees with `apply` for the monads we have already encountered: `Array`, `Maybe` and `Either e`.

If every monad is also an applicative functor, then we should be able to apply our intuition for applicative functors to every monad. In particular, we can reasonably expect a monad to correspond, in some sense, to programming "in a larger language" augmented with some set of additional side-effects. We should be able to lift functions of arbitrary arities, using `map` and `apply`, into this new language.

But monads allow us to do more than we could do with just applicative functors, and the key difference is highlighted by the syntax of do notation. Consider the `userCity` example again, in which we looked for a user's city in an XML document which encoded their user profile:

```haskell
userCity :: XML -> Maybe XML
userCity root = do
  prof <- child root "profile"
  addr <- child prof "address"
  city <- child addr "city"
  pure city
```

Do notation allows the second computation to depend on the result `prof` of the first, and the third computation to depend on the result `addr` of the second, and so on. This dependence on previous values is not possible using only the interface of the `Applicative` type class.

Try writing `userCity` using only `pure` and `apply`: you will see that it is impossible. Applicative functors only allow us to lift function arguments which are independent of each other, but monads allow us to write computations which involve more interesting data dependencies.

In the last chapter, we saw that the `Applicative` type class can be used to express parallelism. This was precisely because the function arguments being lifted were independent of one another. Since the `Monad` type class allows computations to depend on the results of previous computations, the same does not apply - a monad has to combine its side-effects in sequence.

X> ## Exercises
X>
X> 1. (Easy) Look up the types of the `head` and `tail` functions from the `Data.Array` module in the `purescript-arrays` package. Use do notation with the `Maybe` monad to combine these functions into a function `third` which returns the third element of an array with three or more elements. Your function should return an appropriate `Maybe` type.
X> 1. (Medium) Write a function `sums` which uses `foldM` to determine all possible totals that could be made using a set of coins. The coins will be specified as an array which contains the value of each coin. Your function should have the following result:
X>
X>     ```text
X>     > sums []
X>     [0]
X>
X>     > sums [1, 2, 10]
X>     [0,1,2,3,10,11,12,13]
X>     ```
X>
X>     _Hint_: This function can be written as a one-liner using `foldM`. You might want to use the `nub` and `sort` functions to remove duplicates and sort the result respectively.
X> 1. (Medium) Confirm that the `ap` function and the `apply` operator agree for the `Maybe` monad.
X> 1. (Medium) Verify that the monad laws hold for the `Monad` instance for the `Maybe` type, as defined in the `purescript-maybe` package.
X> 1. (Medium) Write a function `filterM` which generalizes the `filter` function on lists. Your function should have the following type signature:
X>
X>     ```haskell
X>     filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
X>     ```
X>
X>     Test your function in PSCi using the `Maybe` and `Array` monads.
X> 1. (Difficult) Every monad has a default `Functor` instance given by:
X>
X>     ```haskell
X>     map f a = do
X>       x <- a
X>       pure (f x)
X>     ```
X>
X>     Use the monad laws to prove that for any monad, the following holds:
X>
X>     ```haskell
X>     lift2 f (pure a) (pure b) = pure (f a b)
X>     ```
X>     
X>     where the `Applicative` instance uses the `ap` function defined above. Recall that `lift2` was defined as follows:
X>    
X>     ```haskell
X>     lift2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c
X>     lift2 f a b = f <$> a <*> b
X>     ```

## Native Effects

We will now look at one particular monad which is of central importance in PureScript - the `Effect` monad.

The `Effect` monad is defined in the `Effect` module. It is used to manage so-called _native_ side-effects. If you are familiar with Haskell, it is the equivalent of the `IO` monad.

What are native side-effects? They are the side-effects which distinguish JavaScript expressions from idiomatic PureScript expressions, which typically are free from side-effects. Some examples of native effects are:

- Console IO
- Random number generation
- Exceptions
- Reading/writing mutable state

And in the browser:

- DOM manipulation
- XMLHttpRequest / AJAX calls
- Interacting with a websocket
- Writing/reading to/from local storage

We have already seen plenty of examples of "non-native" side-effects:

- Optional values, as represented by the `Maybe` data type
- Errors, as represented by the `Either` data type
- Multi-functions, as represented by arrays or lists

Note that the distinction is subtle. It is true, for example, that an error message is a possible side-effect of a JavaScript expression, in the form of an exception. In that sense, exceptions do represent native side-effects, and it is possible to represent them using `Effect`. However, error messages implemented using `Either` are not a side-effect of the JavaScript runtime, and so it is not appropriate to implement error messages in that style using `Effect`. So it is not the effect itself which is native, but rather how it is implemented at runtime.

## Side-Effects and Purity

In a pure language like PureScript, one question which presents itself is: without side-effects, how can one write useful real-world code?

The answer is that PureScript does not aim to eliminate side-effects. It aims to represent side-effects in such a way that pure computations can be distinguished from computations with side-effects in the type system. In this sense, the language is still pure.

Values with side-effects have different types from pure values. As such, it is not possible to pass a side-effecting argument to a function, for example, and have side-effects performed unexpectedly.

The only way in which side-effects managed by the `Effect` monad will be presented is to run a computation of type `Effect a` from JavaScript.

The Pulp build tool (and other tools) provide a shortcut, by generating additional JavaScript to invoke the `main` computation when the application starts. `main` is required to be a computation in the `Effect` monad.

## The Effect Monad

The goal of the `Effect` monad is to provide a well-typed API for computations with side-effects, while at the same time generating efficient JavaScript. 

Here is an example. It uses the `purescript-random` package, which defines functions for generating random numbers:

```haskell
module Main where

import Prelude

import Effect (Effect)
import Effect.Random (random)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  n <- random
  logShow n

```

If this file is saved as `src/Main.purs`, then it can be compiled and run using Pulp:

```text
$ pulp run
```

Running this command, you will see a randomly chosen number between `0` and `1` printed to the console.

This program uses do notation to combine two native effects provided by the JavaScript runtime: random number generation and console IO.

As mentioned previously, the `Effect` monad is of central importance to PureScript. The reason why it's central is because it is the conventional way to interoperate with PureScript's `Foreign Function Interface`, which provides the mechanism to execute a program and perform side effects. While it's desireable to avoid using the `Foreign Function Interface`, it's fairly critical to understand how it works and how to use it, so I recommend reading that chapter before doing any serious PureScript work. That said, the `Effect` monad is fairly simple. It has a few helper functions, but aside from that it doesn't do much except encapsulate side effects. 

## The Aff Monad

The `Aff` monad is an asynchronous effect monad and threading model for PureScipt. 

Asynchrony is typically achieved in JavaScript with callbacks, for example: 

```javascript
function asyncFunction(onSuccess, onError){ ... }
```

The same thing can be modeled with the `Effect` monad: 

```haskell
asyncFunction :: forall success error. (success -> Effect Unit) -> (error -> Effect Unit) -> Effect Unit 
asyncFunction onSucces onError = ...
```

But as is true in JavaScript, this can quickly get out of hand and result in "callback hell". 

The `Aff` monad solves this problem similar to how `Promise` solves it in JavaScript, and there is a great library called `purescript-aff-promise` that provides interop with JavaScript `Promise`.

## Effect to Aff and Aff to Effect

Any synchronous `Effect` can by lifted into an asynchronous `Aff` with `liftEffect`. Similarly, any `Aff` can be converted to an `Effect Unit` with `launchAff_`. Below is the code that prints a random number in terms of `Aff`, written in a few different styles:

```haskell
module Main where

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
main = launchAff_  do
  printRandomStyle1a
  printRandomStyle1b
  printRandomStyle2
  printRandomStyle3

```

`printRandomStyle1a` and `printRandomStyle1b` are nearly the same, but the types more explicit in `printRandomStyle1a` to add additional clarity. In both, the `do` block results in something with type `Effect Unit` and is lifted to `Aff` outside of the `do` block. In `printRandomStyle2`, both `random` and `logShow` are lifted to `Aff` inside the `do` block, which results in an `Aff`. Often while writing PureScript, you'll encounter cases where `Aff` and `Effect` need to be mixed, so style 2 is the more common case. Finally in `printRandomStyle3`, the `liftEffect` function has been moved to the right with `#`, which applies an argument to a function instead of the regular function call with arguments. The purpose of this style is to make the intent of the statment more clear by moving the *boilerplate* out of the way to the right. 

# launchAff_ vs launchAff

`Aff` has two similar functions for converting from an `Aff` to an `Effect`: 

```haskell
launchAff_ :: forall a. Aff a -> Effect Unit
```
```haskell
launchAff :: forall a. Aff a -> Effect (Fiber a)
```

`launchAff` gives back a `Fiber` wrapped in an `Effect`. A `Fiber` is a *forked* computation that can be *joined* back into an `Aff`. You can read more about `Fiber` in Pursuit, PureScript's library and documentation hub. The important thing to note is that there is no direct way to get the contained value in an `Aff` once it's been converted to an `Effect`. For this reason it makes sense to write most of your program in terms of `Aff` instead of `Effect` if you intend to perform asynchonous effects. This may sound limiting, but in practice it is not. Your programs are typically started in the `main` function by wiring up event handlers and listeners, which typically results in a `Unit` and can be run with `launchAff_`. 

# MonadError

`Aff` has an instance of `MonadError`, a type class for clean error handling. `MonadError` is covered in more detail in the *Monadic Adventures* chapter, so below is just a motivating example. 

Imagine you wished to write a `quickCheckout` function by combining several preexisting functions. Without utilizing `MonadError` the code might look like the following:

```haskell
module Main where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, throwError)
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

```

All of the data types and functions (aside from `quickCheckout`) are stubs, and meant to be ignored aside from their types. Note that `quickCheckout` is pretty ugly and the error checking is deeply nested. This is because there is a monad (`Either`) inside of a monad (`Aff`). Monads don't nicely compose so, we've got to step down into each `Aff` and check each `Either`. It's a bit annoying. This is where `MonadError` can help. 

Take a look at the alternate implementation below.


```haskell
module Main where

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

rethrow :: forall a. Aff (Either Error a) -> Aff a
rethrow aff = do
  either <- aff
  case either of
    Left error -> throwError error
    Right a -> pure a

quickCheckout :: Item -> UserInfo -> Aff Receipt
quickCheckout item userInfo = do
  user <- registerUser userInfo # rethrow
  basket <- createBasket user # rethrow
  itemInBasket <- addItemToBasket item basket # rethrow
  purchaseBasket user itemInBasket # rethrow

main :: Effect Unit
main = launchAff_ do
  either <- try $ quickCheckout Item UserInfo
  case either of
    Left error -> log "There was an error checking out!" # liftEffect
    Right _ -> log "Checkout Successful" # liftEffect

```

Note here that `quickCheckout` is much cleaner and the intent of the code is much clearer. This is made possible by the `rethrow` function, which uses `throwError` from `MonadError` to *eliminate* the `Either` type. Your next question might be, "but what happens to the error?". Notice in the `main` function, `try` is called on the result of `quickCheckout`. `try` will catch the error thrown by `throwError` - if one is thrown - and wrap the result in an `Either`, so you can handle it from there. If one doesn't use `try` as is done in the `main` function, then a runtime exception will be thrown. Because you can't really know if upstream code has made use of `MonadError` it's a good idea to call `try` on an `Aff` before converting it into an `Effect`.  



