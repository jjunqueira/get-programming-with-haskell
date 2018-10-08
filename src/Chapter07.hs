module Chapter07 where

mygcd a b =
  if remainder == 0
    then b
    else mygcd b remainder
  where
    remainder = a `mod` b

sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  n -> "a bunch"
