module Data.ReaderTFunctionMaker where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.FunctionMaker ((<<-))
import Type.Equality (class TypeEquals, to)

class ReaderTFunctionMaker input output | input -> output, output -> input where
  readerT :: input -> output

instance args9 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> m a)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args8 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> m a)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args7 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> m a)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args6 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> m a)
  (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args5 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> a4 -> a5 -> m a)
  (a1 -> a2 -> a3 -> a4 -> a5 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args4 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> a4 -> m a)
  (a1 -> a2 -> a3 -> a4 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args3 :: TypeEquals r x => ReaderTFunctionMaker
  (x -> a1 -> a2 -> a3 -> m a)
  (a1 -> a2 -> a3 -> ReaderT r m a)
  where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args2 :: TypeEquals r x => ReaderTFunctionMaker (x -> a1 -> a2 -> m a) (a1 -> a2 -> ReaderT r m a) where
  readerT f = ReaderT <<- (to >>> f $ _)
else
instance args1 :: TypeEquals r x => ReaderTFunctionMaker (x -> a1 -> m a) (a1 -> ReaderT r m a) where
  readerT f = ReaderT <<- (to >>> f $ _)