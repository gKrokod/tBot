module Main (main) where
-- import Data.Either --fromLeft
-- import GHC.Generics
import TData
import qualified ConsoleBot (loop, greeting)
-- import Control.Concurrent
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Text.IO as T
import Data.Function ((&))
import Data.Bool (bool)

buildGetRequest :: Config -> Request
buildGetRequest cfg =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod) 
  $ setRequestSecure (cfg & cSecure)
  $ setRequestQueryString ([("offset", Just $ cfg & cOffset), ("timeout", Just $ cfg & cTimeOut)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/getUpdates"])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest

buildSendRequest :: Config -> TParse -> Request
buildSendRequest cfg user =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod)
  $ setRequestSecure (cfg & cSecure)
  -- $ setRequestQueryString ([("chat_id", user & Just . BC.pack . show . tChatID ), queryMsg $ user & tMessage, ("reply_markup", user & keyboardMenu)  ])
  $ setRequestQueryString ([("chat_id", user & Just . BC.pack . show . tChatID ), queryMsg $ user & tMessage, ("reply_markup", user & keyboardMenu)  ])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, either (\_ -> "/sendMessage") (\_ -> "/sendAnimation") (user & tMessage) ])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest
    where queryMsg (Left msg) = ("text", Just $ E.encodeUtf8 msg)
          queryMsg (Right msg) = ("animation", Just $ E.encodeUtf8 msg)

makeResponse :: Config -> TParse -> TParse
makeResponse cfg user = case user & tMessage of
  Left "/help" -> user {tMessage = Left $ cfg & cTextMenuHelp, isCommand = True}
  Left "/repeat" -> user {tMessage = Left $ cfg & cTextMenuRepeat, keyboardMenu = justKeyboard, isCommand = True}
  Left msg -> user
  Right msg -> user
-- Dopisat' tyt konady repeat, pochitat pro knopki i menu. K tomy ze zdes nado peredavat kolichestvo povtorov
-- добавить в параметры базу данных пользователей, из мейна давать пустой дата мап, а в программе добавлять
-- новых пользователей по айди, если не найден юзер такой.
-- loop :: UserBase -> Config -> UpdateID -> IO ()
buildCallBackQuery :: Config -> QueryID -> Request
buildCallBackQuery cfg q =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod) 
  $ setRequestSecure (cfg & cSecure)
  $ setRequestQueryString ([("callback_query_id", Just $ E.encodeUtf8 $ q), ("text", Just "Your answer is saved"), ("show_alert", Just $ "false")])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/answerCallbackQuery"])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest
loop :: Config -> UpdateID -> IO()
loop cfg updateID = do
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
           L.writeFile "data.json" jsonBody
           loop cfg updateID
	 Just query -> do
	   putStrLn $ show query
	   let updateID' = query & qUpdateID 
	   if updateID == updateID' then loop cfg updateID'
	   else do
  	     response <- httpLBS $ buildCallBackQuery cfg (query & qQueryID)
	     -- putStrLn $ "Get . The status code was: " ++
		--  show (getResponseStatusCode response)
	     -- print $ getResponseHeader "Content-Type" response
	     loop (cfg {cRepeatCount = query & qDataFromButton}) updateID' -- в конфиге ставим новое значение повторов

    Just message -> do
      putStrLn $ show message
      let updateID' = message & tUpdateID 
      if updateID == updateID' then loop cfg updateID'
      else do
        let ourAnswer = makeResponse cfg message
	--todo: тут надо дописать обращение к базе в первом аргументе bool
        let numberCount = bool (fromIntegral $ cfg & cRepeatCount) (1::Int) (ourAnswer & isCommand )
        let echoMessage = replicate numberCount (buildSendRequest cfg ourAnswer)
	
        botResponse' <- mapM httpLBS echoMessage
	let botResponse = head botResponse' -- опасное место с функцией head. будет ли у нас всегда не пустой список здесь?
	
        putStrLn $ "After SEnd The status code was: " ++
          show (getResponseStatusCode botResponse)
        print $ getResponseHeader "Content-Type" botResponse
        LC.putStrLn $ getResponseBody botResponse
        loop cfg updateID'


main :: IO ()
main = do
  mConfig <- loadConfig
  if (mConfig & cMode) == TelegramBot
  then loop mConfig 0 -- if id message don't change then ask again else answer 
  else ConsoleBot.greeting mConfig
