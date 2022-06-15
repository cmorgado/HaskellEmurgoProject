module QuizState(
    answerSession
) where

import System.Random
import Control.Monad.State


answerSession :: Int -> StateT [Int] IO()
answerSession answer = do
    modify (++[answer])


