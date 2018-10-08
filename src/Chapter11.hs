--- Scratch file for chapter 11
module Chapter11 where

x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: String
letters = ['a', 'b', 'c']

aPet :: String
aPet = "cat"

anotherPet ::String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

half :: Int -> Double
half n = fromIntegral n / 2

halve :: Int -> Int
halve value = value `div` 2

printDouble :: Int -> String
printDouble x = show (x * 2)

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
    then f n
    else n

simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

nameTriple = makeTriple "Oscar" 'D' "Grouch"

--- Q11.1  What is the type signature of filter? filter (a -> bool) [a] -> [a]

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs where newInit = f init x
