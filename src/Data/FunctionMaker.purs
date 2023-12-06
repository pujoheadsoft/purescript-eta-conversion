module Data.FunctionMaker
  ( class FunctionMaker
  , makeFrom
  , (<<=)
  , makeFromFlipped
  , (=>>)
  ) where

import Prelude

class FunctionMaker function return constructor | return -> constructor where
  makeFrom :: constructor -> function -> return

instance withInputArgs9 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> o))
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 a7 a8 a9 = constructor $ \i -> function i a1 a2 a3 a4 a5 a6 a7 a8 a9
else
instance args9 :: FunctionMaker
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> o)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 a7 a8 a9 = constructor $ function a1 a2 a3 a4 a5 a6 a7 a8 a9
else
instance withInputArgs8 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> o))
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 a7 a8 = constructor $ \i -> function i a1 a2 a3 a4 a5 a6 a7 a8
else
instance args8 :: FunctionMaker
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> o)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 a7 a8 = constructor $ function a1 a2 a3 a4 a5 a6 a7 a8
else
instance withInputArgs7 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> o))
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 a7 = constructor $ \i -> function i a1 a2 a3 a4 a5 a6 a7
else
instance args7 :: FunctionMaker
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> o)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 a7 = constructor $ function a1 a2 a3 a4 a5 a6 a7
else
instance withInputArgs6 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> o))
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 = constructor $ \i -> function i a1 a2 a3 a4 a5 a6
else
instance args6 :: FunctionMaker
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> o)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 a6 = constructor $ function a1 a2 a3 a4 a5 a6
else
instance withInputArgs5 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> a4 -> a5 -> o))
  (a1 -> a2 -> a3 -> a4 -> a5 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 = constructor $ \i -> function i a1 a2 a3 a4 a5
else
instance args5 :: FunctionMaker
  (a1 -> a2 -> a3 -> a4 -> a5 -> o)
  (a1 -> a2 -> a3 -> a4 -> a5 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 a5 = constructor $ function a1 a2 a3 a4 a5
else
instance withInputArgs4 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> a4 -> o))
  (a1 -> a2 -> a3 -> a4 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 = constructor $ \i -> function i a1 a2 a3 a4
else
instance args4 :: FunctionMaker
  (a1 -> a2 -> a3 -> a4 -> o)
  (a1 -> a2 -> a3 -> a4 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 a4 = constructor $ function a1 a2 a3 a4
else
instance withInputArgs3 :: FunctionMaker
  (i -> (a1 -> a2 -> a3 -> o))
  (a1 -> a2 -> a3 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 a3 = constructor $ \i -> function i a1 a2 a3
else
instance args3 :: FunctionMaker
  (a1 -> a2 -> a3 -> o)
  (a1 -> a2 -> a3 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 a3 = constructor $ function a1 a2 a3
else
instance withInputArgs2 :: FunctionMaker
  (i -> (a1 -> a2 -> o))
  (a1 -> a2 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 a2 = constructor $ \i -> function i a1 a2
else
instance args2 :: FunctionMaker
  (a1 -> a2 -> o)
  (a1 -> a2 -> ret)
  (o -> ret)
  where
  makeFrom constructor function a1 a2 = constructor $ function a1 a2
else
instance withInputArgs1 :: FunctionMaker
  (i -> (a1 -> o))
  (a1 -> ret)
  ((i -> o) -> ret)
  where
  makeFrom constructor function a1 = constructor $ \i -> function i a1
else
instance args1 :: FunctionMaker
  (a1 -> o)
  (a1 -> ret)
  (o -> ret) where
  makeFrom constructor function a1 = constructor $ function a1

infixr 9 makeFrom as <<=

makeFromFlipped :: forall function return constructor. FunctionMaker function return constructor => function -> constructor -> return
makeFromFlipped function constructor = makeFrom constructor function

infixl 9 makeFromFlipped as =>>