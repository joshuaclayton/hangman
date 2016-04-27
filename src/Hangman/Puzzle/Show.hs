module Hangman.Puzzle.Show () where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Hangman.Puzzle (Puzzle(..))

instance Show Puzzle where
    show (Puzzle _ completed' guesses' _) =
        (puzzleState completed') ++ "  |  " ++ (invalidGuessesState guesses')

puzzleState :: [Maybe Char] -> String
puzzleState completed =
    (intersperse ' ' $ fmap renderPuzzleChar completed)
  where
    renderPuzzleChar = fromMaybe '_'

invalidGuessesState :: [Char] -> String
invalidGuessesState guesses =
    (show guessesCount) ++ " invalid " ++ pluralizedGuesses ++ " so far: " ++ guesses
  where
    guessesCount = length guesses
    pluralizedGuesses
        | guessesCount == 1 = "guess"
        | otherwise         = "guesses"
