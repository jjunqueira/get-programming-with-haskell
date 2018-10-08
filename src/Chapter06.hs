module Chapter06 where

import Data.List(elemIndex)

isPalindrome word = word == reverse word

takeLast n aList = reverse (take n (reverse aList))

ones n = take n (cycle[1])

assignToGroups n aList = zip groups aList
  where groups = cycle [1..n]

myrepeat n a = take n (cycle [a])

mysubseq start end aList = take (end - start) (drop start aList)

inFirstHalf e aList = case (elemIndex e aList) of
  Just index -> index < (div (length aList) 2)
  Nothing -> False
