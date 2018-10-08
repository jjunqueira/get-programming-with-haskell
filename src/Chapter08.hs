module Chapter08 where

myDrop n xs = case (n, xs) of
  (0, _ : _) -> xs
  (n, _ : []) -> []
  (n, y : ys) -> myDrop (n - 1) ys

myLength [] = 0
myLength xs = 1 + myLength (tail xs)

myLength2 [] = 0
myLength2 (x:xs) = 1 + myLength2 xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : rest
  where
    rest = myTake (n - 1) xs

finiteCycle (first:rest) = first : rest ++ [first]

myCycle (first:rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n = if even n
  then collatz n `div` 2
  else collatz n*3 + 1

myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
