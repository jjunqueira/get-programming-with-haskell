
--- Chapter 24

{-# LANGUAGE OverloadedStrings #-}

module Chapter24 where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TI
import           System.Environment
import           System.IO

openCloseFile :: IO ()
openCloseFile = do
  myFile <- openFile "hello.txt" ReadMode
  hClose myFile
  putStrLn "done!"

copyFile :: IO ()
copyFile = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"

checkIfFileEmpty :: IO ()
checkIfFileEmpty = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <-
    if not hasLine
      then hGetLine helloFile
      else return "empty"
  putStrLn "done!"

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

fileStats :: IO ()
fileStats = do
  args <- getArgs
  let fileName = head args
  input <- readFile fileName
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary

getCountsT :: T.Text -> (Int, Int, Int)
getCountsT input = (charCount, wordCount, lineCount)
  where charCount = T.length input
        wordCount = (length . T.words) input
        lineCount = (length . T.lines) input

countsTextT :: (Int, Int, Int) -> T.Text
countsTextT (cc, wc, lc) =
  T.pack
    (unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc])

fileStatsT :: IO ()
fileStatsT = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let summary = (countsTextT . getCountsT) input
  TI.appendFile "stats.dat" (mconcat [(T.pack fileName), " ", summary, "\n"])
  TI.putStrLn summary

myCP :: IO ()
myCP = do
  args <- getArgs
  let inFileName = head args
  let outFileName = head (tail args)
  input <- TI.readFile inFileName
  TI.writeFile outFileName input

capitalize :: IO ()
capitalize = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let uppercased = T.toUpper input
  TI.writeFile fileName uppercased
