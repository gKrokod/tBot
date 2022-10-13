module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-- reserv from 14-09-2022
module Main (main) where
import Lib
import TData
import Network.HTTP.Simple (parseRequest, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import Data.Aeson --(decode, Object, (.:))
-- import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L
import Data.Text as T

data TParse = TParse
 {
    tUsername :: T.Text
 ,  tChat_id   :: Integer
 ,  tText     :: T.Text
 ,  tFile_id   :: T.Text
 
 } deriving Show

instance FromJSON TParse where
  parseJSON (Object v) = do
    username    <- v .: "result" 
                   >>= \m -> Prelude.head m .: "message" 
                   >>= (.: "chat")  
                   >>= (.: "username")  
    chatid      <- v .: "result" 
                   >>= \m -> Prelude.head m .: "message" 
                   >>= (.: "chat")  
                   >>= (.: "id")  
    textMessage <- v .: "result" 
                   >>= \m -> Prelude.head m .: "message" 
                   >>= \t -> t .:? "text" .!= "NotText" 
    fileid      <- v .: "result" 
                   >>= \m -> Prelude.head m .: "message" 
                   >>= (.:? "animation")  
                   >>= \f -> case f of
                               Nothing -> return "NotGif" 
                               Just f' -> (f' .: "file_id") .!= "NotGif"

    return $ TParse { tUsername = username
                    , tChat_id  = chatid 
                    , tText     = textMessage
                    , tFile_id  = fileid --fileid
 }
    -- TParse <$> v .: "ok"
    --        <*> v .: "ok"
    --        <*> v .: "ok"
    --        <*> v .: "ok"

myToken :: BC.ByteString
myToken = "5703519544:AAGMFTqId4rwfly47AkqmgN-tswUH52z67U"

botHost :: BC.ByteString
botHost = "api.telegram.org"

apiPath :: BC.ByteString
apiPath = "/bot"

getUpdates :: BC.ByteString
getUpdates = "/getUpdates?offset=-1" -- (-1) It's last message

-- urlGet build url for request api.telegram.org
urlGet :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> String
urlGet host api token request = BC.unpack $ mconcat ["https://", host, api, token, request]

sendTestMessage :: BC.ByteString
sendTestMessage = "/sendMessage?chat_id=9829858&text=KykyHaskellKyKy"

sendTestAnimation :: BC.ByteString
sendTestAnimation = "/sendAnimation?chat_id=9829858&animation=CgACAgQAAxkBAAMaYyGbhr9QGtT2GjLtelwmBLIDO9oAAuMCAAJF6HRTBvZVEThS-WgpBA"

-- getBlogRepoURL :: Object -> String
-- getBlogRepoURL info =
--     case parseMaybe extractBlogRepoURL info of
--         Nothing  -> ""
--         Just url -> url
--     where
--         extractBlogRepoURL = \info -> info .: "result" 
--           >>= \m -> Prelude.head m .: "message" 
--           >>= (.: "text")  


-- getTParse :: Object -> TParse
-- getTParse info = do
--   v1 <- info .: "result" >>= \m -> head m .: "message" >>= (.: "text")  
--   TParse {message = v1}


main :: IO ()
main = do
  urlUpdate <- parseRequest $ urlGet botHost apiPath myToken getUpdates
  response <- httpLBS urlUpdate 
  putStrLn $ "The status code was: " ++
       show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  let jsonBody = getResponseBody (response)
  LC.putStrLn $ jsonBody
  L.writeFile "data.json" jsonBody

  let result = decode jsonBody :: Maybe TParse
  putStrLn $ case result of
    Nothing -> "vava nothing"
    Just info -> show info

--   -- rawJSON <- B.readFile "data.json"
--   let result = decode jsonBody :: Maybe Object
-- -- json всегда верный будет, по параметру ок от телеги. поэтому проверка на nothing будет редка или сомнительна. надо подумать
--   putStrLn $ case result of
--     Nothing   -> "Invalid JSON!"
--     Just info -> mconcat [getBlogRepoURL info, "boom"]

  -- urlUpdate <- parseRequest $ urlGet botHost apiPath myToken sendTestAnimation
  -- response <- httpLBS urlUpdate
  --
  -- putStrLn $ "The status code was: " ++
  --      show (getResponseStatusCode response)
  -- print $ getResponseHeader "Content-Type" response
  -- LC.putStrLn $ getResponseBody response
--}
