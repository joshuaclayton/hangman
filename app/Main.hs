module Main where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Hangman.RandomWord (pickRandomWord)
import Hangman.Puzzle

main :: IO ()
main = do
    word <- pickRandomWord
    let puzzle = newPuzzle word
    runGame puzzle

runGame :: Puzzle -> IO ()
runGame p = forever $ do
    update p >>= runGame

update :: Puzzle -> IO Puzzle
update p = do
    renderPuzzle p
    putStrLn $ stateAnnouncement p

    case state p of
        Won              -> exitSuccess
        Lost             -> exitSuccess
        InvalidGuess   _ -> return $ p { state = Playing }
        AlreadyGuessed _ -> return $ p { state = Playing }
        _                -> do
                            guess <- captureGuess
                            return $ handleInput p guess

captureGuess :: IO String
captureGuess = do
    putStrLn "Guess a letter: "
    getLine

renderPuzzle :: Puzzle -> IO ()
renderPuzzle p = do
    putStrLn $ "\n" ++ show p ++ "\n"
