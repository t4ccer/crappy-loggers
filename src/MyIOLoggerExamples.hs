module MyIOLoggerExamples where

import           MyIOLogger
import           System.Random

type Log = String
type Logs = [Log]

genIOValue :: MyIOLogger Logs Int
genIOValue = appendIOLogs print ["Generated random value"] $ fromIO randomIO

divideIOValues :: Int -> Int -> MyIOLogger Logs Int
divideIOValues a b
  | b == 0 = failWithIOLogs print ["Division by 0 error"]
  | otherwise = returnWithIOLogs print ["Divided values"] $ a `div` b

showIOValue :: Int -> MyIOLogger Logs String
showIOValue = returnWithIOLogs print ["Showed value"] . show

myIOLoggerExample1 :: MyIOLogger Logs String
myIOLoggerExample1 = do
  x <- genIOValue
  y <- divideIOValues x 2
  showIOValue y

myIOLoggerExample2 :: MyIOLogger Logs String
myIOLoggerExample2 = do
  x <- genIOValue
  y <- divideIOValues x 0
  showIOValue y
