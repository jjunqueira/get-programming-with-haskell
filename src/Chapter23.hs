--- Chapter 23

{-# LANGUAGE OverloadedStrings #-}

module Chapter23 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Semigroup

--- firstWord :: String
--- firstWord = "pessimism"

--- secondWord :: T.Text
--- secondWord = T.pack firstWord

--- thirdWord :: String
--- thirdWord = T.unpack secondWord

--- fourthWord :: T.Text
--- fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

combineTextMonoid :: T.Text
combineTextMonoid = mconcat ["some", " ","text"]

combineTextSemigroup :: T.Text
combineTextSemigroup = "some" <> " " <> "text"

myLines :: T.Text -> [T.Text]
myLines text = T.splitOn "\n" text

myUnlines :: [T.Text] -> T.Text
myUnlines textLines = T.intercalate "\n" textLines

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{", query, "}"]

searchProgram = do
  TIO.putStrLn (highlight dharma bgText)

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

helloProgram :: IO ()
helloProgram = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

