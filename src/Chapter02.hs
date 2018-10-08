module Chapter02 where

calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

doublePlusTwo x = doubleX + 2
  where
    doubleX = x * 2

inc n = n + 1

double n = n * 2

square n = n ^ 2

testFunc n =
  if even n
    then n - 2
    else 3 * n + 1
