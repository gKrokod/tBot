module TData where

import GHC.Generics
import Data.Text as T
import qualified Data.ByteString.Char8 as BC

type Gif = T.Text
type Message = T.Text
type ConfigKey = T.Text
type ChatID = Integer
type UpdateID = Integer
type DataFromButton = Integer
type QueryID = T.Text

data Mode = ConsoleBot | TelegramBot deriving (Show, Eq)

data TParse = TParse
 {
    tUpdateID :: UpdateID
 ,  tChatID :: ChatID
 ,  tMessage :: Either Message Gif
 ,  keyboardMenu :: Maybe BC.ByteString
 ,  isCommand :: Bool
 } deriving Show

data TParseQuery = TParseQuery 
 {
   qUpdateID :: UpdateID
 , qChatID :: ChatID
 , qQueryID :: QueryID
 , qDataFromButton :: DataFromButton
 } deriving Show

data Config = Config
 {
    cRepeatCount :: Integer
 ,  cTextMenuHelp :: T.Text  -- check for Russian words
 ,  cTextMenuRepeat :: T.Text -- check for Russian words
 ,  cApiPath :: BC.ByteString
 ,  cBotHost :: BC.ByteString
 ,  cTimeOut :: BC.ByteString
 ,  cOffset :: BC.ByteString
 ,  cToken :: BC.ByteString
 ,  cPort :: Int
 ,  cMethod :: BC.ByteString
 ,  cSecure :: Bool
 ,  cMode :: Mode
 } deriving Show

data Keyboard = Keyboard {
                  inline_keyboard :: [[Button]]	        
                } deriving (Show, Generic)

data Button = Button {
                text :: T.Text
	      , callback_data :: T.Text	
              } deriving (Show, Generic)

