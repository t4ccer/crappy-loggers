module MyLoggerExamples where

import           MyLogger

type Log = String
type Logs = [Log]

genValue :: MyLogger Logs Int
genValue = returnWithLogs ["Generated value"] 42

divideValues :: Int -> Int -> MyLogger Logs Int
divideValues a b
  | b == 0 = failWithLogs ["Division by 0 error"]
  | otherwise = returnWithLogs ["Divided values"] $ a `div` b

showValue :: Int -> MyLogger Logs String
showValue = returnWithLogs ["Showed value"] . show

myLoggerExample1 :: MyLogger Logs String
myLoggerExample1 = do
  x <- genValue
  y <- divideValues x 2
  showValue y

myLoggerExample2 :: MyLogger Logs String
myLoggerExample2 = do
  x <- genValue
  y <- divideValues x 0
  showValue y
