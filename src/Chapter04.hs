module Chapter04 where

import Data.List

ifEvenInc n =
  if even n
    then n + 1
    else n

ifEvenDouble n =
  if even n
    then n * 2
    else n

ifEvenSquare n =
  if even n
    then n ^ 2
    else n

ifEven myFunction x =
  if even x
    then myFunction x
    else x

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenIncHigherOrder n = ifEven inc n

ifEvenDoubleHigherOrder n = ifEven double n

ifEvenSquareHigherOrder n = ifEven square n

ifEvenCube n = ifEven (\x -> x ^ 3) n

author = ("Will", "Kurt")

names =
  [ ("Ian", "Curtis")
  , ("Bernard", "Summer")
  , ("Peter", "Hook")
  , ("Stephen", "Morris")
  ]

compareLastNames name1 name2 =
  if lastName1 > lastName2
    then GT
    else if lastName1 < lastName2
           then LT
           else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2


addressLetter name location = nameText ++ " - " ++ location
  where
    nameText = (fst name) ++ " " ++ (snd name)


sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = (snd name)
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ":PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = (fst name) ++ " " ++ (snd name) ++ " Esq"

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter2 name location = locationFunction name
  where
    locationFunction = getLocationFunction location

compareLastNames2 name1 name2 = compare name1 name2

main = do
  print $ sort names
  print $ sortBy compareLastNames names

