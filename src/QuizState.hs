module QuizState(
    answerSession, writeToScreenState
) where

import System.Random
import Control.Monad.State


answerSession :: Int -> StateT [Int] IO()
answerSession answer = do
    modify (++[answer])


writeToScreenState :: StateT String IO ()
writeToScreenState = do
        state <- get
        lift $ putStrLn state    