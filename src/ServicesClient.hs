module ServicesClient (getQuestions)
    
 where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Char (toLower)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import RIO ( (>>>), LByteString, Generic )
import QuizTypes ( Question )

jsonOptions :: String -> JSON.Options
jsonOptions prefix =
    let prefixLength = length prefix
        lowercaseFirstCharacter (c : rest) = toLower c : rest
        lowercaseFirstCharacter [] = []
    in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter}

getQuizContent :: IO LByteString
getQuizContent = do
    manager <- newTlsManager
    request <- HTTP.parseRequest "https://my-json-server.typicode.com/cmorgado/HaskellEmurgoProject/questions"
    HTTP.responseBody <$> HTTP.httpLbs request manager


getQuestions :: IO (Either String [Question])
getQuestions = do
    manager <- newTlsManager
    request <- HTTP.parseRequest "https://my-json-server.typicode.com/cmorgado/HaskellEmurgoProject/questions"
    (HTTP.responseBody >>> JSON.eitherDecode) <$> HTTP.httpLbs request manager
