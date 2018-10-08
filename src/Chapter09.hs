module Chapter09 where

import Data.Char(toLower)

add3ToAll [] = []
add3ToAll (x:xs) = (3 + x) : add3ToAll xs

mul3ByAll [] = []
mul3ByAll (x:xs) = (3 + x) : mul3ByAll xs

addAnA [] = []
addAnA (x:xs) = ("a " ++ x) : addAnA (xs)

squareAll [] = []
squareAll (x:xs) = x ^ 2 : squareAll xs

myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

onlyA xs = filter (\(x:xs) -> x == 'a') xs

myFilter test [] = []
myFilter test (x:xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

myRemove test [] = []
myRemove test (x:xs) =
  if test x
    then myRemove test xs
    else x : myRemove test xs

myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfQuares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x

myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs

myElem [] e = False
myElem (x:xs) e = filteredLength > 0
  where
    filteredLength = length (filter (\y -> x == y) xs)

isPalindrome word = cleanedWord == reverse cleanedWord
  where cleanedWord = filter (\x -> x /= ' ') (map toLower word)

harmonic n = foldr (+) (0) (map (1/) [1..n])
