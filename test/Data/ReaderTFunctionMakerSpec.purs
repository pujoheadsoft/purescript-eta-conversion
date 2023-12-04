module Test.Data.ReaderTFunctionMakerSpec where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.ReaderTFunctionMaker (readerT)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = do
  describe "make from (with input)" do
    it "args 1" do
      let
        f = readerT _.m1
        g = \a1 -> ReaderT \r -> r.m1 a1
      v <- runReaderT (f "a") functions
      w <- runReaderT (g "a") functions
      v `shouldEqual` w

    it "args 2" do
      let
        f = readerT _.m2
        g = \a1 a2 -> ReaderT $ \r -> r.m2 a1 a2
      v <- runReaderT (f "a" 1) functions
      w <- runReaderT (g "a" 1) functions
      v `shouldEqual` w

    it "args 3" do
      let
        f = readerT _.m3
        g = \a1 a2 a3 -> ReaderT \r -> r.m3 a1 a2 a3
      v <- runReaderT (f "a" 1 true) functions
      w <- runReaderT (g "a" 1 true) functions
      v `shouldEqual` w

    it "args 4" do
      let 
        f = readerT _.m4
        g = \a1 a2 a3 a4 -> ReaderT \r -> r.m4 a1 a2 a3 a4
      v <- runReaderT (f "a" 1 true "b") functions
      w <- runReaderT (g "a" 1 true "b") functions
      v `shouldEqual` w

    it "args 5" do
      let 
        f = readerT _.m5
        g = \a1 a2 a3 a4 a5 -> ReaderT \r -> r.m5 a1 a2 a3 a4 a5
      v <- runReaderT (f "a" 1 true "b" 2) functions
      w <- runReaderT (g "a" 1 true "b" 2) functions
      v `shouldEqual` w

    it "args 6" do
      let 
        f = readerT _.m6
        g = \a1 a2 a3 a4 a5 a6 -> ReaderT \r -> r.m6 a1 a2 a3 a4 a5 a6
      v <- runReaderT (f "a" 1 true "b" 2 false) functions
      w <- runReaderT (g "a" 1 true "b" 2 false) functions
      v `shouldEqual` w

    it "args 7" do
      let 
        f = readerT _.m7
        g = \a1 a2 a3 a4 a5 a6 a7 -> ReaderT \r -> r.m7 a1 a2 a3 a4 a5 a6 a7
      v <- runReaderT (f "a" 1 true "b" 2 false "c") functions
      w <- runReaderT (g "a" 1 true "b" 2 false "c") functions
      v `shouldEqual` w

    it "args 8" do
      let 
        f = readerT _.m8
        g = \a1 a2 a3 a4 a5 a6 a7 a8 -> ReaderT \r -> r.m8 a1 a2 a3 a4 a5 a6 a7 a8
      v <- runReaderT (f "a" 1 true "b" 2 false "c" 3) functions
      w <- runReaderT (g "a" 1 true "b" 2 false "c" 3) functions
      v `shouldEqual` w

    it "args 9" do
      let 
        f = readerT _.m9
        g = \a1 a2 a3 a4 a5 a6 a7 a8 a9 -> ReaderT \r -> r.m9 a1 a2 a3 a4 a5 a6 a7 a8 a9
      v <- runReaderT (f "a" 1 true "b" 2 false "c" 3 true) functions
      w <- runReaderT (g "a" 1 true "b" 2 false "c" 3 true) functions
      v `shouldEqual` w

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

functions :: { 
  m1 :: String -> Aff String, 
  m2 :: String -> Int -> Aff String, 
  m3 :: String -> Int -> Boolean -> Aff String, 
  m4 :: String -> Int -> Boolean -> String -> Aff String, 
  m5 :: String -> Int -> Boolean -> String -> Int -> Aff String, 
  m6 :: String -> Int -> Boolean -> String -> Int -> Boolean -> Aff String, 
  m7 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Aff String, 
  m8 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> Aff String, 
  m9 :: String -> Int -> Boolean -> String -> Int -> Boolean -> String -> Int -> Boolean -> Aff String
}
functions = {
  m1,
  m2,
  m3,
  m4,
  m5,
  m6,
  m7,
  m8,
  m9
}
