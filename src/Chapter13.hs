module Chapter13 where

addThenDouble:: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving(Show, Eq, Ord)