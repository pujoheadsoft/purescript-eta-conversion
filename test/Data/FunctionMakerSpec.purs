module Test.Data.FunctionMakerSpec where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

class FunctionMaker function return constructor | 
  return -> constructor
  where
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
instance withInputArgs5 :: FunctionMaker (i -> (a1 -> a2 -> a3 -> a4 -> a5 -> o)) (a1 -> a2 -> a3 -> a4 -> a5 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 a2 a3 a4 a5 = constructor $ \i -> function i a1 a2 a3 a4 a5
else
instance args5 :: FunctionMaker (a1 -> a2 -> a3 -> a4 -> a5 -> o) (a1 -> a2 -> a3 -> a4 -> a5 -> ret) (o -> ret) where
  makeFrom constructor function a1 a2 a3 a4 a5 = constructor $ function a1 a2 a3 a4 a5
else
instance withInputArgs4 :: FunctionMaker (i -> (a1 -> a2 -> a3 -> a4 -> o)) (a1 -> a2 -> a3 -> a4 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 a2 a3 a4 = constructor $ \i -> function i a1 a2 a3 a4
else
instance args4 :: FunctionMaker (a1 -> a2 -> a3 -> a4 -> o) (a1 -> a2 -> a3 -> a4 -> ret) (o -> ret) where
  makeFrom constructor function a1 a2 a3 a4 = constructor $ function a1 a2 a3 a4
else
instance withInputArgs3 :: FunctionMaker (i -> (a1 -> a2 -> a3 -> o)) (a1 -> a2 -> a3 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 a2 a3 = constructor $ \i -> function i a1 a2 a3
else
instance args3 :: FunctionMaker (a1 -> a2 -> a3 -> o) (a1 -> a2 -> a3 -> ret) (o -> ret) where
  makeFrom constructor function a1 a2 a3 = constructor $ function a1 a2 a3
else
instance withInputArgs2 :: FunctionMaker (i -> (a1 -> a2 -> o)) (a1 -> a2 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 a2 = constructor $ \i -> function i a1 a2
else
instance args2 :: FunctionMaker (a1 -> a2 -> o) (a1 -> a2 -> ret) (o -> ret) where
  makeFrom constructor function a1 a2 = constructor $ function a1 a2
else
instance withInputArgs1 :: FunctionMaker (i -> (a1 -> o)) (a1 -> ret) ((i -> o) -> ret) where
  makeFrom constructor function a1 = constructor $ \i -> function i a1
else
instance args1 :: FunctionMaker (a1 -> o) (a1 -> ret) (o -> ret) where
  makeFrom constructor function a1 = constructor $ function a1

infix 9 makeFrom as <<-



spec :: Spec Unit
spec = do
  describe "make from" do
    it "args 1" do
      let
        f = Data <<- f1
        g = \a -> Data $ f1 a
      f "a" `shouldEqual` g "a"
    
    it "args 2" do
      let
        f = Data <<- f2
        g = \a1 a2 -> Data $ f2 a1 a2
      f "a" 1 `shouldEqual` g "a" 1

    it "args 3" do
      let 
        f = Data <<- f3
        g = \a1 a2 a3 -> Data $ f3 a1 a2 a3
      f "a" 1 true `shouldEqual` g "a" 1 true

    it "args 4" do
      let 
        f = Data <<- f4
        g = \a1 a2 a3 a4 -> Data $ f4 a1 a2 a3 a4
      f "a" 1 true "b" `shouldEqual` g "a" 1 true "b"

    it "args 5" do
      let 
        f = Data <<- f5
        g = \a1 a2 a3 a4 a5 -> Data $ f5 a1 a2 a3 a4 a5
      f "a" 1 true "b" 2 `shouldEqual` g "a" 1 true "b" 2

    it "args 6" do
      let 
        f = Data <<- f6
        g = \a1 a2 a3 a4 a5 a6 -> Data $ f6 a1 a2 a3 a4 a5 a6
      f "a" 1 true "b" 2 false `shouldEqual` g "a" 1 true "b" 2 false

    it "args 7" do
      let 
        f = Data <<- f7
        g = \a1 a2 a3 a4 a5 a6 a7 -> Data $ f7 a1 a2 a3 a4 a5 a6 a7
      f "a" 1 true "b" 2 false "c" `shouldEqual` g "a" 1 true "b" 2 false "c"

    it "args 8" do
      let 
        f = Data <<- f8
        g = \a1 a2 a3 a4 a5 a6 a7 a8 -> Data $ f8 a1 a2 a3 a4 a5 a6 a7 a8
      f "a" 1 true "b" 2 false "c" 3 `shouldEqual` g "a" 1 true "b" 2 false "c" 3

    it "args 9" do
      let 
        f = Data <<- f9
        g = \a1 a2 a3 a4 a5 a6 a7 a8 a9 -> Data $ f9 a1 a2 a3 a4 a5 a6 a7 a8 a9
      f "a" 1 true "b" 2 false "c" 3 true `shouldEqual` g "a" 1 true "b" 2 false "c" 3 true

  describe "make from (with input)" do
    it "args 1" do
      let
        f = Data <<- \(Functions r) -> r.f1
        g = \a1 -> Data $ \(Functions r) -> r.f1 a1
        v = runData (f "a") functions
        w = runData (g "a") functions
      v `shouldEqual` w

    it "args 2" do
      let
        f = Data <<- \(Functions r) -> r.f2
        g = \a1 a2 -> Data $ \(Functions r) -> r.f2 a1 a2
        v = runData (f "a" 1) functions
        w = runData (g "a" 1) functions
      v `shouldEqual` w

    it "args 3" do
      let
        f = Data <<- \(Functions r) -> r.f3
        g = \a1 a2 a3 -> Data $ \(Functions r) -> r.f3 a1 a2 a3
        v = runData (f "a" 1 true) functions
        w = runData (g "a" 1 true) functions
      v `shouldEqual` w

    it "args 4" do
      let 
        f = Data <<- \(Functions r) -> r.f4
        g = \a1 a2 a3 a4 -> Data $ \(Functions r) -> r.f4 a1 a2 a3 a4
        v = runData (f "a" 1 true "b") functions
        w = runData (g "a" 1 true "b") functions
      v `shouldEqual` w

    it "args 5" do
      let 
        f = Data <<- \(Functions r) -> r.f5
        g = \a1 a2 a3 a4 a5 -> Data $ \(Functions r) -> r.f5 a1 a2 a3 a4 a5
        v = runData (f "a" 1 true "b" 2) functions
        w = runData (g "a" 1 true "b" 2) functions
      v `shouldEqual` w

    it "args 6" do
      let 
        f = Data <<- \(Functions r) -> r.f6
        g = \a1 a2 a3 a4 a5 a6 -> Data $ \(Functions r) -> r.f6 a1 a2 a3 a4 a5 a6
        v = runData (f "a" 1 true "b" 2 false) functions
        w = runData (g "a" 1 true "b" 2 false) functions
      v `shouldEqual` w

    it "args 7" do
      let 
        f = Data <<- \(Functions r) -> r.f7
        g = \a1 a2 a3 a4 a5 a6 a7 -> Data $ \(Functions r) -> r.f7 a1 a2 a3 a4 a5 a6 a7
        v = runData (f "a" 1 true "b" 2 false "c") functions
        w = runData (g "a" 1 true "b" 2 false "c") functions
      v `shouldEqual` w

    it "args 8" do
      let 
        f = Data <<- \(Functions r) -> r.f8
        g = \a1 a2 a3 a4 a5 a6 a7 a8 -> Data $ \(Functions r) -> r.f8 a1 a2 a3 a4 a5 a6 a7 a8
        v = runData (f "a" 1 true "b" 2 false "c" 3) functions
        w = runData (g "a" 1 true "b" 2 false "c" 3) functions
      v `shouldEqual` w

    it "args 9" do
      let 
        f = Data <<- \(Functions r) -> r.f9
        g = \a1 a2 a3 a4 a5 a6 a7 a8 a9 -> Data $ \(Functions r) -> r.f9 a1 a2 a3 a4 a5 a6 a7 a8 a9
        v = runData (f "a" 1 true "b" 2 false "c" 3 true) functions
        w = runData (g "a" 1 true "b" 2 false "c" 3 true) functions
      v `shouldEqual` w

data Data a = Data a
derive instance genericData :: Generic (Data a) _
instance showData :: Show a => Show (Data a) where
  show = genericShow
instance eqData :: Eq a => Eq (Data a) where
  eq = genericEq

runData :: forall a. Data a -> a
runData (Data a) = a

f1 :: String -> String
f1 a = "[" <> a <> "]"
f2 :: String -> Int -> String
f2 a b = "[" <> a <> show b <> "]"
f3 :: String -> Int -> Boolean -> String
f3 a b c = "[" <> a <> show b <> show c <> "]"
f4 :: String -> Int -> Boolean -> String -> String
f4 a b c d = "[" <> a <> show b <> show c <> d <> "]"
f5 :: String -> Int -> Boolean -> String -> Int -> String
f5 a b c d e = "[" <> a <> show b <> show c <> d <> show e <> "]"
f6 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String
f6 a b c d e f = "[" <> a <> show b <> show c <> d <> show e <> show f <> "]"
f7 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> String
f7 a b c d e f g = "[" <> a <> show b <> show c <> d <> show e <> show f <> g <> "]"
f8 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> String
f8 a b c d e f g h = "[" <> a <> show b <> show c <> d <> show e <> show f <> g <> show h <> "]"
f9 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> Boolean -> String
f9 a b c d e f g h i = "[" <> a <> show b <> show c <> d <> show e <> show f <> g <> show h <> show i <> "]"

m1 :: String -> Aff String
m1 a = pure (f1 a)
m2 :: String -> Int -> Aff String
m2 a b = pure $ f2 a b
m3 :: String -> Int -> Boolean -> Aff String
m3 a b c = pure $ f3 a b c
m4 :: String -> Int -> Boolean -> String -> Aff String
m4 a b c d = pure $ f4 a b c d
m5 :: String -> Int -> Boolean -> String -> Int -> Aff String
m5 a b c d e = pure $ f5 a b c d e
m6 :: String -> Int -> Boolean -> String -> Int -> Boolean -> Aff String
m6 a b c d e f = pure $ f6 a b c d e f
m7 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Aff String
m7 a b c d e f g = pure $ f7 a b c d e f g
m8 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> Aff String
m8 a b c d e f g h = pure $ f8 a b c d e f g h
m9 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> Boolean -> Aff String
m9 a b c d e f g h i = pure $ f9 a b c d e f g h i

newtype Functions = Functions {
  f1 :: String -> String,
  f2 :: String -> Int -> String,
  f3 :: String -> Int -> Boolean -> String,
  f4 :: String -> Int -> Boolean -> String -> String,
  f5 :: String -> Int -> Boolean -> String -> Int -> String,
  f6 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String,
  f7 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> String,
  f8 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> String,
  f9 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> Boolean -> String
}

functions :: Functions
functions = Functions {
  f1,
  f2,
  f3,
  f4,
  f5,
  f6,
  f7,
  f8,
  f9
}
