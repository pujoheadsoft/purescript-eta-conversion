module Main where

import Prelude

import Data.FunctionMaker ((<<:), (<<|))
import Data.Maybe (Maybe(..))

example1 :: forall a. Semigroup a => a -> a -> Maybe a
example1 v1 v2 = Just $ v1 <> v2

example2 :: forall a. Semigroup a => a -> a -> Maybe a
example2 = Just <<| (<>)

example3 :: forall a. Semigroup a => a -> Maybe (a -> a)
example3 a = Just $ \v -> v <> a

example4 :: forall a. Semigroup a => a -> Maybe (a -> a)
example4 = Just <<: (<>)