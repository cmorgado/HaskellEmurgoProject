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
    putStrLn("   " ++ show (i + 1) ++ ") " ++answerAnswer x)
    loopAnswers xs (i + 1)


loopQuestions :: [Question] -> [Question] -> [Int] -> Int ->  IO ()
loopQuestions [] qs a i = do
    putStrLn "------------------------------------------------------------------"
    let answerValue =  (100.0::Double) / fromIntegral (length qs )
    calculateResult qs a 0 answerValue 0
    putStrLn "------------------------------------------------------------------"


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




