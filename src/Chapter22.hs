--- Chapter 22

module Chapter22 where

import System.Environment
import Control.Monad
import Data.List.Split

getArgsTest :: IO ()
getArgsTest = do
  args <- getArgs
  mapM_ putStrLn args

exampleMain :: IO ()
exampleMain = do
  vals <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn vals

printExample :: IO ()
printExample = do
  args <- getArgs
  let linesToRead =
        if length args > 0
          then read (head args)
          else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)
  
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n func = mapM (\_ -> func) [1..n]

testGetContents :: IO ()
testGetContents = do
  userInput <- getContents
  mapM_ print userInput

testReverseInput :: IO ()
testReverseInput = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

testLinesInput :: IO ()
testLinesInput = do
  userInput <-  getContents
  let numbers = toInts userInput
  print (sum numbers)

mainSumSquares :: IO ()
mainSumSquares = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^2) numbers
  print (sum squares)

data MyOperator = Sum | Multiply
type Equation = (Int, MyOperator, Int)

parseOp :: String -> MyOperator
parseOp "+" = Sum
parseOp "*" = Multiply

parseEquation :: String -> Equation
parseEquation input = (firstNum, op, lastNum)
  where splitEq = splitOn " " input
        (firstNum, firstRemoved) = (read (head splitEq), (tail splitEq))
        (op, opRemoved) = (parseOp (head firstRemoved), (tail firstRemoved))
        lastNum = read (head opRemoved)

runEquation :: Equation -> Int
runEquation (a, Sum, b) = a + b
runEquation (a, Multiply, b) = a * b

toEquations :: [String] -> [Equation]
toEquations input = map parseEquation input

simpleCalc :: IO ()
simpleCalc = do
  input <- getContents
  let program = runEquation . parseEquation
  mapM_ (\line -> print (program line)) (lines input)

getQuote :: String -> String
getQuote "1" = "Test 1"
getQuote "2" = "Test 2"
getQuote "3" = "Test 3"
getQuote "4" = "Test 4"
getQuote "5" = "Test 5"
getQuote _ = "Quote Not Found"

quoteProgramDesc :: String
quoteProgramDesc = "Pick a quote to print ( 1 through 5 ) or type 'n' to quit"

quotesProgram :: IO ()
quotesProgram = do
  putStrLn quoteProgramDesc
  input <- getContents
  let inStream = takeWhile (\quote -> quote /= "n") (lines input)
  mapM_ (\input -> putStrLn ((getQuote input) ++ "\n" ++ quoteProgramDesc)) inStream
