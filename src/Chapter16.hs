
--- Scratch file for Chapter 16

module Chapter16 where

data BreakfastSide
  = Toast
  | Biscuit
  | Homefries
  | Fruit
  deriving (Show)

data BreakfastMeat
  = Sausage
  | Bacon
  | Ham
  deriving (Show)

data BreakfastMain
  = Egg
  | Pancake
  | Waffle
  deriving (Show)

data AuthorName = AuthorName
  { firstName :: String
  , lastName :: String
  }

data Car = Car
data Spoiler = Spoiler
data SportsCar = SportsCar Car Spoiler

type FirstName = String

type MiddleName = String

type LastName = String

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName
  | TwoInitialWithLast Char
                       Char
                       LastName
  | FirstNameWithTwoInits FirstName
                          Char
                          Char deriving (Show)

data Author =
  Author Name deriving(Show)

data Artist
  = Person Name
  | Band String deriving (Show)

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  | PamphletCreator Author
  deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialWithLast 'H' 'P' "Lovecraft"))

data Book = Book
  { author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: String
  , bookPrice :: Double
  }

data VinlyRecord = VinlyRecord
  { artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name :: String
  , description :: String
  , toyPrice :: Double
  }

data Pamphlet = Pamphlet
  { title :: String
  , pamphletDescription :: String
  , contact :: Creator
  }

data StoreItem
  = BookItem Book
  | RecordItem VinlyRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy (PamphletItem pamphlet) = show (contact pamphlet)
madeBy _ = "unknown"

data Circle = Circle
  { radius :: Double
  }

data Square = Square
  { side :: Double
  }

data Rectangle = Rectangle
  { width :: Double
  , height :: Double
  }

data Shape =  CircleShape Circle | SquareShape Square | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape circle) = 2 * 3.14 * (radius circle)
perimeter (SquareShape square) = 4 * (side square)
perimeter (RectangleShape rec) = 2 * ((width rec) + (height rec))
