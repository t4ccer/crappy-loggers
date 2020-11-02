{-# LANGUAGE ScopedTypeVariables #-}

module RIOLogger where

import Control.Monad.Reader
import MyIOLogger

type RIOLogger r w a = ReaderT r (MyIOLogger w) a

runRIOLogger :: Monoid w => RIOLogger r w a -> r -> IO (Maybe a, w)
runRIOLogger r env = runMyIOLogger $ runReaderT r env

appendRIOLogs :: Monoid w => (w -> IO ()) -> w -> RIOLogger r w a -> RIOLogger r w a
appendRIOLogs f w = mapReaderT (appendIOLogs f w)

