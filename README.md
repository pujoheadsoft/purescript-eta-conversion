# purescript-eta-conversion

[![Latest release](http://img.shields.io/github/release/pujoheadsoft/purescript-eta-conversion.svg)](https://github.com/pujoheadsoft/purescript-eta-conversion/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-eta-conversion/badge)](https://pursuit.purescript.org/packages/purescript-eta-conversionpujoheadsoft)
[![CI](https://github.com/pujoheadsoft/purescript-eta-conversion/workflows/CI/badge.svg)](https://github.com/pujoheadsoft/purescript-eta-conversion/actions?query=workflow%3ACI+branch%3Amaster)


Libraries that support eta conversion.

Generates a new function that can be eta conversion from function.

[日本語版 README](https://github.com/pujoheadsoft/purescript-eta-conversion/blob/master/README-ja.md)

## instration
Install with [Spago](https://github.com/purescript/spago):
```
spago install eta-conversion
```

## Usage
### Basics
Suppose you have the following data structure `Result` and function `fun` and a function called `exampleA1` that uses them.
```haskell
newtype Result a = Result a

fun :: String -> Int -> Boolean -> String
fun _ _ _ = ""

exampleA1 :: String -> Int -> Boolean -> Result String
exampleA1 s i b = Result $ fun s i b
```
By using the `<<|` operator for this `exampleA1` function, the following eta conversion can be performed.
```haskell
import Data.EtaConversionTransformer ((<<|))

exampleA2 :: String -> Int -> Boolean -> Result String
exampleA2 = Result <<| fun
```
This works the same as if you had written.
```haskell
exampleA2 = \s i b -> Result $ fun s i b
```
That is, `a <<| f` creates a new function that has the same arguments as function `f` and returns the result of applying those arguments to `f` and passing it to function `a`.

`<<|` signature as follows when there is only one argument.
```haskell
(o -> ret) -> (a1 -> o) -> (a1 -> ret)
```
If there are two arguments, signature is as follows.
```haskell
(o -> ret) -> (a1 -> a2 -> o) -> (a1 -> a2 -> ret)
```
When there are three or more arguments, the number of arguments in the signature increases in the same pattern as above.

`<<|` supports up to 9 arguments.

### Generate a new function with left-rotated order of argument definitions
The `rotate` function can be used to generate a new function with a shifted argument definition as follows.
```haskell
import Data.ArgsRotater (rotate)

fun :: String -> Int -> Boolean -> String
fun _ _ _ = ""

example :: Boolean -> String -> Int -> String
example = rotate fun
```
The `<^` operator can be used to create a new function with the last argument already applied.
```haskell
import Data.ArgsRotater ((<^))

fun :: String -> Int -> Boolean -> String
fun _ _ _ = ""

example :: String -> Int -> String
example = fun <^ true -- last args applied function
```

### Generate a 'function that generates a data structure with a  function'
Suppose we have the following data structures `Result`,`Functions`, a function `runFunctions` to retrieve the contents of `Functions`, and a function `exampleB1` to use them.
```haskell
newtype Result a = Result a

newtype Functions = Functions {
  fun :: String -> Int -> Boolean -> String
}

runFunctions :: Functions -> { fun :: String -> Int -> Boolean -> String }
runFunctions (Functions r) = r

exampleB1 :: String -> Int -> Boolean -> Result (Functions -> String)
exampleB1 s i b = Result $ (\f -> f.fun s i b) <<< runFunctions
```
Using the `<<:` operator, this `exampleB1` can be defined as follows.
```haskell
import Data.EtaConversionTransformer ((<<:))

exampleB2 :: String -> Int -> Boolean -> Result (Functions -> String)
exampleB2 = Result <<: _.fun <<< runFunctions
```
### Generate a "function to generate a ReaderT that would take a function out of the environment and use it"
Suppose we want to generate a ReaderT that takes a record with a function as its environment and uses that function internally as follows.
```haskell
exampleC1 :: forall r m. TypeEquals r { fun :: String -> m Unit } => String -> ReaderT r m Unit
exampleC1 s = ReaderT $ \r -> (to r).fun s
```
Such a function can be written using the `readerT` function as follows.
```haskell
import Data.ReaderTEtaConversionTransformer (readerT)

exampleC2 :: forall r m. TypeEquals r { fun :: String -> m Unit } => String -> ReaderT r m Unit
exampleC2 = readerT _.fun
```
