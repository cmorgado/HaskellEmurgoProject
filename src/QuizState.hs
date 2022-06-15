module QuizState(
    mainS,answerSession,getSession
) where

import System.Random
import Control.Monad.State

mainS :: IO ()
mainS = do
    news <- execStateT (answerSession 1) []
    guesses <- execStateT (answerSession 11) []
    putStrLn $ "Success in " ++ (show guesses) ++ " tries."
    x <- execStateT(getSession 1) []
    putStrLn $ show x 
 --   answer <- getStdRandom (randomR (1,100)) -- think of a number
 --   putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
 --   guesses <- execStateT (guessSession answer) 0
 --   putStrLn $ "Success in " ++ (show guesses) ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer = do
    gs <- lift getLine    -- get guess from user
    let g = read gs       -- convert to number
    modify (+1)           -- increment number of guesses
    case compare g answer of
        LT -> do
            lift $ putStrLn "Too low"
            guessSession answer
        GT -> do
            lift $ putStrLn "Too high"
            guessSession answer
        EQ -> lift $ putStrLn "Got it!"



answerSession :: Int -> StateT [Int] IO()
answerSession answer = do
    modify (++[answer])


getSession :: Int -> StateT [Int] IO[Int]
getSession x = do
    get

io :: IO a -> StateT [Integer] IO a
io = liftIO
