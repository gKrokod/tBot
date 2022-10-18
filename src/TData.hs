module TData where

import Data.Aeson --(Object (..), (.:))
import Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as C
import Data.Bool (bool)

type Gif = T.Text
type Message = T.Text
type ConfigKey = T.Text
type ChatID = Integer
type UpdateID = Integer

data Mode = ConsoleBot | TelegramBot deriving (Show, Eq)

data TParse = TParse
 {
    tUpdateID :: UpdateID
 ,  tChatID :: ChatID
 ,  tMessage :: Either Message Gif
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


lookUpConfig :: ConfigKey -> IO T.Text
lookUpConfig key = do
  conf <- C.load [C.Required "config/bot.cfg"]
  C.lookupDefault "NotFoundInBot.cfg" conf key
   
loadConfig :: IO Config
loadConfig = do
  conf <- C.load [C.Required "config/bot.cfg"]
  rcount<- C.lookupDefault "NotFoundRepeatCount.cfg" conf ("config.user.repeatcount")
  helpmenu <- C.lookupDefault "NotFoundHelpMenu.cfg" conf ("config.user.helpmenu")
  repeatmenu <- C.lookupDefault "NotFoundRepeatMenu.cfg" conf ("config.user.repeatmenu")
  apipath <- C.lookupDefault "NotFoundApiPath.cfg" conf ("config.url.apipath")
  bothost <- C.lookupDefault "NotFoundBotHost.cfg" conf ("config.url.bothost")
  timeout <- C.lookupDefault "NotFoundTimeOut.cfg" conf ("config.url.timeout")
  offset <- C.lookupDefault "NotFoundOffset.cfg" conf ("config.url.offset")
  token <- C.lookupDefault "NotFoundToken.cfg" conf ("config.url.token")
  port <- C.lookupDefault "NotFoundPort.cfg" conf ("config.url.port")
  method <- C.lookupDefault "NotFoundMethod.cfg" conf ("config.url.method")
  secure <- C.lookupDefault False conf ("config.url.secure")
  mode <- C.lookupDefault False conf ("config.telegrammode")
  return $ Config { 
                    cRepeatCount = read $ T.unpack rcount 
                  , cTextMenuHelp = helpmenu
		  , cTextMenuRepeat = repeatmenu
		  , cApiPath = E.encodeUtf8 apipath
		  , cBotHost = E.encodeUtf8 bothost
		  , cTimeOut = E.encodeUtf8 timeout
		  , cOffset = E.encodeUtf8 offset
                  , cToken = E.encodeUtf8 token
                  , cPort = read $ T.unpack port
                  , cMethod = E.encodeUtf8 method
                  , cSecure = secure
		  , cMode = bool ConsoleBot TelegramBot mode
		  }

