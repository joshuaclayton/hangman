module Main where

import Control.Monad ((<=<), forever)
import Hangman.Puzzle
import Hangman.RandomWord (pickRandomWord)
import System.Exit (exitSuccess)

main :: IO ()
main = runGame =<< newPuzzle <$> pickRandomWord

runGame :: Puzzle -> IO ()
runGame = forever . runGame <=< update

update :: Puzzle -> IO Puzzle
update p = do
    renderPuzzle p
    putStrLn $ stateAnnouncement p
    case pState p of
        Won -> exitSuccess
        Lost -> exitSuccess
        InvalidGuess _ -> return $ p {pState = Playing}
        AlreadyGuessed _ -> return $ p {pState = Playing}
        _ -> do
            guess <- captureGuess
            return $ handleInput p guess

captureGuess :: IO String
captureGuess = do
    putStrLn "Guess a letter: "
    getLine

renderPuzzle :: Puzzle -> IO ()
renderPuzzle p = putStrLn $ "\n" ++ show p ++ "\n"
