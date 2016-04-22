module Hangman.RandomWord
    ( pickRandomWord
    ) where

import System.Random (randomRIO)

type WordList = [String]

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
    filter (\w -> elem (length w) [5..9]) wl
