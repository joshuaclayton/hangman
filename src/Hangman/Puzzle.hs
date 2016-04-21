module Hangman.Puzzle
    ( Puzzle (state)
    , PuzzleState (..)
    , stateAnnouncement
    , newPuzzle
    , handleInput
    ) where

import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)

data PuzzleState = NotStarted | Playing | InvalidGuess String | AlreadyGuessed Char | Won | Lost

data Puzzle = Puzzle
    { answer :: String
    , completed :: [Maybe Char]
    , guesses :: [Char]
    , state :: PuzzleState
    }

newPuzzle :: String -> Puzzle
newPuzzle answer =
    Puzzle answer (map (const Nothing) answer) [] NotStarted

stateAnnouncement :: Puzzle -> String
stateAnnouncement puzzle =
    case state puzzle of
        Won -> "You've won!"
        Lost -> "You've lost; better luck next time! The correct answer is '" ++ answer puzzle ++ "'."
        InvalidGuess guess -> "Please only guess a single letter! (You guessed '" ++ guess ++ "')"
        AlreadyGuessed guess -> "You already guessed '" ++ [guess] ++ "'"
        _ -> ""

handleInput :: Puzzle -> String -> Puzzle
handleInput puzzle input =
    case input of
        [c] -> handleGuess puzzle c
        _   -> puzzle { state = InvalidGuess input }

handleGuess :: Puzzle -> Char -> Puzzle
handleGuess puzzle guess =
    newPuzzle { state = newState }
  where
    (Puzzle _ completed' guesses' _) = puzzle
    newPuzzle = puzzle { guesses = (guess : guesses'), completed = newFilledInSoFar }
    newState
        | all isJust (completed newPuzzle) = Won
        | length (guesses newPuzzle) > 7 = Lost
        | elem guess guesses' = AlreadyGuessed guess
        | otherwise = Playing
    zipper guessed wordChar guessChar =
        case wordChar == guessed of
            True  -> Just wordChar
            False -> guessChar
    newFilledInSoFar =
        zipWith (zipper guess) (answer puzzle) completed'

instance Show Puzzle where
    show (Puzzle _ completed' guesses' _) =
        (intersperse ' ' $ fmap renderPuzzleChar completed') ++ "  |  Guessed so far: " ++ guesses'
      where
        renderPuzzleChar = fromMaybe '_'
