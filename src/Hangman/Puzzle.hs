module Hangman.Puzzle
    ( Puzzle (state)
    , PuzzleState (..)
    , stateAnnouncement
    , newPuzzle
    , handleInput
    ) where

import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.List (intersperse, nub, (\\))

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
handleInput puzzle [c]   = handleGuess puzzle c
handleInput puzzle input = puzzle { state = InvalidGuess input }

handleGuess :: Puzzle -> Char -> Puzzle
handleGuess puzzle@(Puzzle _ completed' guesses' _) guess =
    newPuzzle { state = newState }
  where
    newPuzzle = puzzle { guesses = newGuesses, completed = newCompleted }
    newState
        | all isJust (completed newPuzzle) = Won
        | length (guesses newPuzzle) > 7   = Lost
        | guess `elem` previousGuesses     = AlreadyGuessed guess
        | otherwise                        = Playing
    newCompleted = newFilledInSoFar puzzle guess
    newGuesses = nub (guess : guesses') \\ catMaybes newCompleted
    previousGuesses = guesses' ++ catMaybes completed'

newFilledInSoFar :: Puzzle -> Char -> [Maybe Char]
newFilledInSoFar (Puzzle answer' completed' _ _) guess =
    zipWith (zipper guess) answer' completed'
  where
    zipper guessed wordChar guessChar =
        case wordChar == guessed of
            True  -> Just wordChar
            False -> guessChar

instance Show Puzzle where
    show (Puzzle _ completed' guesses' _) =
        (intersperse ' ' $ fmap renderPuzzleChar completed') ++ "  |  Guessed so far: " ++ guesses'
      where
        renderPuzzleChar = fromMaybe '_'
