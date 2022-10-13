module TData where

import Data.Aeson --(Object (..), (.:))
import Data.Text as T
import qualified Data.ByteString.Char8 as BC

type Gif = T.Text
type Message = T.Text
type ChatID = Integer
type UpdateID = Integer

data TParse = TParse
 {
    tUpdateID :: UpdateID
 ,  tChatID :: ChatID
 ,  tMessage :: Either Message Gif
 } deriving Show

instance FromJSON TParse where
  parseJSON (Object v) = do
    updateId <- v .: "result" 
                  >>= \m -> Prelude.head m .: "update_id" 
    chatId   <- v .: "result" 
                  >>= \m -> Prelude.head m .: "message" 
                  >>= (.: "chat")  
                  >>= (.: "id")  
    message  <- v .: "result"
                  >>= \m -> Prelude.head m .: "message" 
                  >>= \t -> t .:? "text"
                  >>= \case 
                        Just message -> return $ Left message
                        Nothing -> t .: "animation"
                          >>= (.: "file_id") 
                          >>= \gif -> return $ Right gif
 
    return $ TParse { 
                      tUpdateID = updateId
                    , tChatID   = chatId 
                    , tMessage  = message 
                    }

myTimeOut :: Maybe BC.ByteString
myTimeOut = Just "5"

myOffset :: Maybe BC.ByteString
myOffset = Just "-1"

myToken :: BC.ByteString
myToken = ""

botHost :: BC.ByteString
botHost = "api.telegram.org"

apiPath :: BC.ByteString
apiPath = "/bot"
