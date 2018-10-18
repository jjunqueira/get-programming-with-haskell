--- Chapter 21 Notes

module Chapter21 where

import           System.Random
import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

sayHello :: IO ()
sayHello = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

rollDie :: IO ()
rollDie = do
  dieRoll <- randomRIO (minDie, maxDie)
  putStrLn (show dieRoll)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The " ++
  show size ++
  " pizza " ++ "is cheaper at " ++ show costSqInch ++ " per square inch"
  where
    costSqInch = costPerInch (size, cost)

pizzaProgram :: IO ()
pizzaProgram = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

pizzaMaybeProgram :: Maybe String
pizzaMaybeProgram = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

names :: Map.Map Int String
names = Map.fromList [(1, "Josh")]

maybeSayHello :: Maybe String
maybeSayHello = do
  name <- Map.lookup 1 names
  let statement = helloPerson name
  return statement

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibProgram :: IO ()
fibProgram = do
  putStrLn "Which Fibonacci number would you like to see?"
  num <- getLine
  let fibSequence = fib (read num)
  putStrLn (show fibSequence)
