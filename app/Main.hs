module Main (main) where
import TData
import Config (loadConfig, numberForCommand)
import Parse (justKeyBoard)
import qualified ConsoleBot (loop, greeting)
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import Data.Aeson (decode)
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as E (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Text.IO as T
import Data.Function ((&))
import Data.Bool (bool)
import HttpMessage (buildGetRequest, buildSendRequest, buildCallBackQuery, makeResponse)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Map.Internal.Debug
-- to do
type UserDB = Map.Map ChatID RepeatCount

-- modifyUserDB :: UserDB -> ChatID -> UserDB
-- modifyUserDB base user = bool (Map.insert user 3 base) (base) (Map.member user base)

loop :: UserDB -> Config -> UpdateID -> IO()
loop base cfg updateID = do
  response <- httpLBS $ buildGetRequest cfg 
  putStrLn $ "Get . The status code was: " ++
       show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  let jsonBody = getResponseBody (response)
  -- LC.putStrLn $ jsonBody
  -- L.writeFile "data.json" jsonBody
  let mbMessage = decode jsonBody :: Maybe TParse
  case mbMessage of -- Maybe there is a message?
    Nothing -> do
       let mbQuery = decode jsonBody :: Maybe TParseQuery
       case mbQuery of -- Maybe there is a query (from Button)?
         Nothing -> do
           putStrLn "JSON is uncorrect"
           LC.putStrLn $ jsonBody
           L.writeFile "log/data.json" jsonBody
           loop base cfg updateID
	 Just query -> do
	   putStrLn $ show query
	   let updateID' = query & qUpdateID 
	   if updateID == updateID' then loop base cfg updateID'
	   else do
  	     response <- httpLBS $ buildCallBackQuery cfg (query & qQueryID)
	     -- putStrLn $ "Get . The status code was: " ++
		--  show (getResponseStatusCode response)
	     -- print $ getResponseHeader "Content-Type" response
             let base' = Map.insert (query & qChatID) (query & qDataFromButton) base
             -- L.writeFile "log/userDB.txt" (LC.pack (showTree base'))
	     loop base' cfg updateID' 

    Just message -> do
      let base' = bool (Map.insert (message & tChatID) (cfg & cRepeatCount) base) (base) (Map.member (message & tChatID) base)
      putStrLn $ show message
      let updateID' = message & tUpdateID 
      if updateID == updateID' then loop base' cfg updateID'
      else do
        let ourAnswer = makeResponse cfg message
        let numberFromBase = fromMaybe (cfg & cRepeatCount) (Map.lookup (message & tChatID) base')
        let numberCount = bool numberFromBase numberForCommand (ourAnswer & isCommand )
        let echoMessage = replicate numberCount (buildSendRequest cfg ourAnswer)
	
        botResponse' <- mapM httpLBS echoMessage
	let botResponse = head botResponse' -- опасное место с функцией head. будет ли у нас всегда не пустой список здесь?
	
        putStrLn $ "After SEnd The status code was: " ++
          show (getResponseStatusCode botResponse)
        print $ getResponseHeader "Content-Type" botResponse
        LC.putStrLn $ getResponseBody botResponse
        loop base' cfg updateID'

main :: IO ()
main = do
  mConfig <- loadConfig
  if (mConfig & cMode) == TelegramBot
  then loop  (Map.empty) mConfig 0 -- if id message don't change then ask again else answer 
  else ConsoleBot.greeting mConfig
