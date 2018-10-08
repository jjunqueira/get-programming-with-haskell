module Chapter14Solutions where

class Ord a => Die a where
  winner :: a -> a -> String
  winner d1 d2 =
    if d1 > d2
      then "left"
      else "right"

data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving(Eq, Ord, Show, Enum)

instance Die FiveSidedDie