module Common where
import System.Console.ANSI
import Control.Concurrent
import Data.List
import Control.Exception
import Text.Read
import QuizState
import Control.Monad.State

welcoming :: IO ()
welcoming =
  do
    h0 "---------------------------------------------------"
    h0 "           W E L C O  M E   T O  HASQUIZ           "
    h0 "           Quizes to let  you understand           "
    h0 "           your knowledge about  HASKELL           "
    h0 "---------------------------------------------------"

h0 :: String -> IO ()
h0 a = do 
    putStrLn a
    return ()

h1 :: String -> IO ()
h1 a = do 
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn a
    setSGR [Reset]
    return ()

h2 :: String -> IO ()
h2 a = do 
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn a
    setSGR [Reset]
    return ()

h2Error :: String -> IO ()
h2Error a = do 
    setSGR [SetColor Foreground Vivid Red]
    putStrLn a
    setSGR [Reset]
    return ()

h2Correct :: String -> IO ()
h2Correct a = do 
    setSGR [SetColor Foreground Vivid Green]
    putStrLn a
    setSGR [Reset]   
    return () 


promptReset :: IO ()
promptReset =  setSGR [Reset]

promptAnswerNumber :: String -> Int -> IO Int
promptAnswerNumber answer max = do
    h2 answer 
    response <-  getLine
    case readEither response of
        Right e -> do
            if e `elem` [1 .. max]
            then  return (read response)
            else do
                h2Error ("Please insert a valid answer 1 to " ++ show max)
                promptAnswerNumber answer max
        Left e -> do 
            h2Error ("Please insert a valid answer 1 to " ++ show max)
            promptAnswerNumber answer max



