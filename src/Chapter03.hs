module Chapter03 where

sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

body sumSquare squareSum =
  if sumSquare > squareSum
    then sumSquare
    else squareSum

sumSquareOrSquareSum2 x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

sumSquareOrSquareSum3 x y =
  (\sumSquare squareSum ->
     if sumSquare > squareSum
       then sumSquare
       else squareSum)
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

doubleDouble x = dubs * 2
  where
    dubs = x * 2


doubleDoubleLamda x = ((\y -> y * 2) x) * 2
