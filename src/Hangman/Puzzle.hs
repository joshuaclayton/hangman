module Hangman.Puzzle
    ( Puzzle(Puzzle, pState)
    , PuzzleState(..)
    , stateAnnouncement
    , newPuzzle
    , handleInput
    ) where

import Data.List ((\\), intersperse, nub)
import Data.Maybe (catMaybes, fromMaybe, isJust)

data PuzzleState
    = NotStarted
    | Playing
    | InvalidGuess String
    | AlreadyGuessed Char
    | Won
    | Lost

data Puzzle = Puzzle
    { pAnswer :: String
    , pCompleted :: [Maybe Char]
    , pGuesses :: String
    , pState :: PuzzleState
    }

instance Show Puzzle where
    show (Puzzle _ completed' guesses' _) =
        puzzleState completed' ++ "  |  " ++ invalidGuessesState guesses'

puzzleState :: [Maybe Char] -> String
puzzleState completed = intersperse ' ' $ fmap renderPuzzleChar completed
  where
    renderPuzzleChar = fromMaybe '_'

invalidGuessesState :: String -> String
invalidGuessesState guesses =
    show guessesCount ++ " invalid " ++ pluralizedGuesses ++ " so far: " ++ guesses
  where
    guessesCount = length guesses
    pluralizedGuesses
        | guessesCount == 1 = "guess"
        | otherwise = "guesses"

newPuzzle :: String -> Puzzle
newPuzzle answer = Puzzle answer (map (const Nothing) answer) [] NotStarted

stateAnnouncement :: Puzzle -> String
stateAnnouncement puzzle =
    case pState puzzle of
        Won -> "You've won!"
        Lost ->
            "You've lost; better luck next time! The correct answer is '" ++ pAnswer puzzle ++ "'."
        InvalidGuess guess -> "Please only guess a single letter! (You guessed '" ++ guess ++ "')"
        AlreadyGuessed guess -> "You already guessed '" ++ [guess] ++ "'"
        _ -> ""

handleInput :: Puzzle -> String -> Puzzle
handleInput puzzle [c] = handleGuess puzzle c
handleInput puzzle input = puzzle {pState = InvalidGuess input}

handleGuess :: Puzzle -> Char -> Puzzle
handleGuess puzzle@(Puzzle _ completed' guesses' _) guess = p {pState = newState}
  where
    p = puzzle {pGuesses = newGuesses, pCompleted = newCompleted}
    newState
        | all isJust (pCompleted p) = Won
        | length (pGuesses p) > 7 = Lost
        | guess `elem` previousGuesses = AlreadyGuessed guess
        | otherwise = Playing
    newCompleted = newFilledInSoFar puzzle guess
    newGuesses = nub (guess : guesses') \\ catMaybes newCompleted
    previousGuesses = guesses' ++ catMaybes completed'

newFilledInSoFar :: Puzzle -> Char -> [Maybe Char]
newFilledInSoFar (Puzzle answer' completed' _ _) guess = zipWith (zipper guess) answer' completed'
  where
    zipper guessed wordChar guessChar =
        if wordChar == guessed
            then Just wordChar
            else guessChar
