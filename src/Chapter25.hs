--- Chapter 25

{-# LANGUAGE OverloadedStrings #-}

module Chapter25 where

import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           System.Environment
import           System.Random
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal


randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO (0, 255)
  return (toEnum randomInt)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse target

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReplaceByte
                , randomSortSection
                , randomReplaceByte
                , randomSortSection
                , randomReplaceByte]

glitcher :: IO ()
glitcher = do
  args <- getArgs
  let fileName = head args
  imageFile <-BC.readFile fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

byteCharDiff :: IO ()
byteCharDiff = do
  args <- getArgs
  let fileName = head args
  contents <- TIO.readFile fileName
  let charLength = T.length contents
  let byteLength = BC.length (E.encodeUtf8 contents)
  putStrLn (mconcat ["Byte length: ", show byteLength, "; Characters: ", show charLength])
