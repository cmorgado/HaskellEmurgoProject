{-# LANGUAGE BlockArguments #-}
module Lib (runMain) where
import QuizTypes
import QuizState
import Common
import ServicesClient
import System.Random
import System.Console.ANSI
import Control.Monad.State

runMain :: IO ()
runMain = do
    welcoming
    maybeQuestions <- getQuestions
    case maybeQuestions of
        Right questions -> do
            let questionsContent = map questionQuestion questions
            loopQuestions questions questions [] 0
        Left e -> error e


loopAnswers :: [Answer] -> Int -> IO Int
loopAnswers [] i = do
    promptAnswerNumber "Choose the correct:" i
loopAnswers (x:xs) i = do
    h0 ("   " ++ show (i + 1) ++ ") " ++answerAnswer x)
    loopAnswers xs (i + 1)


loopQuestions :: [Question] -> [Question] -> [Int] -> Int ->  IO ()
loopQuestions [] qs a i = do
    h0 "------------------------------------------------------------------"
    let answerValue =  (100.0::Double) / fromIntegral (length qs )
    calculateResult qs a 0 answerValue 0
    h0 "------------------------------------------------------------------"
    showUserAnsweredQuestion qs a 0
loopQuestions (x:xs) qs a idx = do
    h1 (show (idx + 1) ++ ") " ++questionQuestion x)
    responseFromUser <- loopAnswers (questionAnswers x) 0
    execStateT (answerSession responseFromUser) []
    loopQuestions xs qs (a ++ [responseFromUser]) (idx+1)


calculateResult :: [Question] -> [Int] -> Int -> Double -> Double -> IO Double
calculateResult [] answers idx val res = do
    if res<50 then
        h2Error ("your result was:"++ show res ++"%")
    else
        h2Correct ("your result was:"++ show res ++"%")
    return res
calculateResult (q:qs) answers idx val res = do
    let as = questionAnswers q
    let answerIdx = answers !! idx
    let correct = loopQuestionsResult as (answerIdx-1)
    if correct then
        do
            calculateResult qs answers (idx+1) val (val+res)
    else
        do
            calculateResult qs answers (idx+1) val res



loopQuestionsResult :: [Answer] -> Int -> Bool
loopQuestionsResult as idx =  do
    answerCorrect (as !! idx)




showUserAnsweredQuestion :: [Question] -> [Int] -> Int ->  IO()
showUserAnsweredQuestion [] [] i = do
    h1 "------------------------------------------------------------"
    return()
showUserAnsweredQuestion (x:xs) (y:ys) i = do
    h1 (show (i + 1) ++ ") " ++questionQuestion x)
    showUserAnsweredQuestionAnswersAndResult (questionAnswers x) y 0
    showUserAnsweredQuestion xs ys (i+1)


showUserAnsweredQuestionAnswersAndResult :: [Answer] -> Int -> Int -> IO()
showUserAnsweredQuestionAnswersAndResult [] i idx = do
    h1 "------------------------------------------------------------"
    return()
showUserAnsweredQuestionAnswersAndResult (x:xs) i idx = do
    if answerCorrect x
        then do
            h2Correct ("   " ++ show (idx + 1) ++ ") " ++answerAnswer x)
    else do
        if idx+1 == i then do
            h2Error ("   " ++ show (idx + 1) ++ ") " ++answerAnswer x)
        else do
            h0 ("   " ++ show (idx + 1) ++ ") " ++answerAnswer x)

    showUserAnsweredQuestionAnswersAndResult xs i (idx+1)


