{-# LANGUAGE DeriveFunctor #-}

module MyIOLogger where

import           Control.Exception
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy
import           MyLogger

newtype MyIOLogger w a = MyIOLogger (IO (MyLogger w a))
  deriving Functor

instance Monoid w => Applicative (MyIOLogger w) where
  pure = MyIOLogger . return . return
  MyIOLogger f <*> MyIOLogger a = MyIOLogger $ do
    f' <- f
    a' <- a
    return $ f' <*> a'

instance Monoid w => Monad (MyIOLogger w) where
  MyIOLogger a >>= f = MyIOLogger $ do
    (a', w1) <- runMyLogger <$> a

    let MyIOLogger b = case a' of
            Just v  -> f v
            Nothing -> MyIOLogger $ return mzero

    (b', w2) <- runMyLogger <$> b
    return $ appendLogs (w1<>w2) $ fromMaybe b'

runMyIOLogger :: Monoid w => MyIOLogger w a -> IO (Maybe a, w)
runMyIOLogger (MyIOLogger a) = fmap runMyLogger a

fromIO :: Monoid w => IO a -> MyIOLogger w a
fromIO = MyIOLogger . fmap return

liftLogger :: MyLogger w a -> MyIOLogger w a
liftLogger = MyIOLogger . return

appendIOLogs :: Monoid w => (w -> IO ()) -> w -> MyIOLogger w a -> MyIOLogger w a
appendIOLogs f ls (MyIOLogger a) = MyIOLogger $ do
  _  <- f ls
  appendLogs ls <$> a

returnWithIOLogs :: Monoid w => (w -> IO ()) -> w -> a -> MyIOLogger w a
returnWithIOLogs f ls = appendIOLogs f ls . return

failWithIOLogs :: Monoid w => (w -> IO ()) -> w -> MyIOLogger w a
failWithIOLogs f ls = appendIOLogs f ls $ liftLogger $ fromMaybe Nothing
