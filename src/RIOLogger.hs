module RIOLogger where

import           Control.Monad.Reader
import           MyIOLogger
import qualified MyIOLogger           as IOL

type RIOLogger r w a = ReaderT r (MyIOLogger w) a

runRIOLogger :: Monoid w => RIOLogger r w a -> r -> IO (Maybe a, w)
runRIOLogger r env = runMyIOLogger $ runReaderT r env

appendRIOLogs :: Monoid w => (w -> IO ()) -> w -> RIOLogger r w a -> RIOLogger r w a
appendRIOLogs f w = mapReaderT (appendIOLogs f w)

failRIOLogs :: Monoid w => (w -> IO ()) -> w -> RIOLogger r w a
failRIOLogs f w = ReaderT (const $ failWithIOLogs f w)

fromIO :: Monoid w => IO a -> RIOLogger r w a
fromIO io = ReaderT $ const (IOL.fromIO io)
