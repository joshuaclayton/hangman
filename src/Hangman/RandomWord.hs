module Hangman.RandomWord
    ( pickRandomWord
    ) where

import System.Random (randomRIO)
import Data.Char (isUpper)

type WordList = [String]
type WordFilter = (String -> Bool)

pickRandomWord :: IO String
pickRandomWord =
    allWords >>= randomWord

allWords :: IO WordList
allWords =
    readFile "data/dict.txt" >>= return . gameWords . lines

randomWord :: WordList -> IO String
randomWord wl = do
    idx <- randomRIO (lowerBounds, upperBounds)
    return $ wl !! idx
  where
    lowerBounds = 0
    upperBounds = (length wl) - 1

gameWords :: WordList -> WordList
gameWords wl =
    foldr filter wl gameFilters

gameFilters :: [WordFilter]
gameFilters = [not . properNoun, inRange [5..9]]

inRange :: [Int] -> WordFilter
inRange r = (`elem` r) . length

properNoun :: WordFilter
properNoun [] = False
properNoun (x:_) = isUpper x
