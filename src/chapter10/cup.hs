--- Coffee cup modelling
module Cup where

cup flOz = \message -> message flOz
getOz aCup = aCup (\flOz -> flOz)
drink aCup ozDrank = cup (max 0 (flOz - ozDrank))
  where flOz = getOz aCup
isEmpty aCup = getOz aCup == 0

coffeeCup = cup 12
