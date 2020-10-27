{-# LANGUAGE LambdaCase #-}

module MyLogger where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Lazy

type MyLogger w a = MaybeT (Writer w) a

runMyLogger :: Monoid w => MyLogger w a -> (Maybe a, w)
runMyLogger = runWriter . runMaybeT

appendLogs :: Monoid w => w -> MyLogger w a -> MyLogger w a
appendLogs s = (>>) $ tell s

failWithLogs :: Monoid w => w -> MyLogger w a
failWithLogs ls = appendLogs ls mzero

fromMaybe :: Monoid w => Maybe a -> MyLogger w a
fromMaybe = \case
  Just a  -> return a
  Nothing -> mzero

returnWithLogs :: Monoid w => w -> a -> MyLogger w a
returnWithLogs ls a = appendLogs ls $ return a
