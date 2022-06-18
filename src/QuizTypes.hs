{-# LANGUAGE DeriveGeneric #-}
module QuizTypes where 

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Char (toLower)
import RIO ( (>>>), LByteString, Generic )

jsonOptions :: String -> JSON.Options
jsonOptions prefix = 
    let prefixLength = length prefix
        lowercaseFirstCharacter (c : rest) = toLower c : rest
        lowercaseFirstCharacter [] = []
    in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter} 

data Answer = Answer  {
    answerAnswer :: String,
    answerCorrect :: Bool
} deriving (Eq, Show, Generic)

instance FromJSON Answer where 
    parseJSON = JSON.genericParseJSON $ jsonOptions "answer"

data Question = Question {
    questionQuestion :: String,
    questionResponse :: Int,
    questionAnswers :: [Answer]
} deriving (Eq, Show, Generic)

instance FromJSON Question where 
    parseJSON = JSON.genericParseJSON $ jsonOptions "question"

data WriteStyle = H1 | H2 | H3 | H4 | H5 | H6 | P 
