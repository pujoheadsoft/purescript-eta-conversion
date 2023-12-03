module Data.FunctionMaker
  ( class FunctionMaker
  , makeFrom
  , (<<-)
  ) where

import Prelude

class FunctionMaker function return constructor | function -> return, return -> function where
  makeFrom :: constructor -> function -> return

instance withInputArgs3 :: FunctionMaker (i -> (a1 -> a2 -> a3 -> o)) (a1 -> a2 -> a3 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 a2 a3 = constructor $ \i -> function i a1 a2 a3
else
instance args3 :: FunctionMaker (a1 -> a2 -> a3 -> o) (a1 -> a2 -> a3 -> m o) (o -> m o) where
  makeFrom constructor function a1 a2 a3 = constructor $ function a1 a2 a3
else
instance withInputArgs2 :: FunctionMaker (i -> (a1 -> a2 -> o)) (a1 -> a2 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 a2 = constructor $ \i -> function i a1 a2
else
instance args2 :: FunctionMaker (a1 -> a2 -> o) (a1 -> a2 -> m o) (o -> m o) where
  makeFrom constructor function a1 a2 = constructor $ function a1 a2
else
instance withInputArgs1 :: FunctionMaker (i -> (a1 -> o)) (a1 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 = constructor $ \i -> function i a1
else
instance args1 :: FunctionMaker (a1 -> o) (a1 -> m o) (o -> m o) where
  makeFrom constructor function a1 = constructor $ function a1

infix 9 makeFrom as <<-
