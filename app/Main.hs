module Main (main) where
-- import Data.Either --fromLeft
import TData
import qualified ConsoleBot (loop, greeting)
-- import Control.Concurrent
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Text.IO as T
import Data.Function ((&))

buildSendRequest :: Config -> TParse -> Request
buildSendRequest cfg user =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod)
  $ setRequestSecure (cfg & cSecure)
  $ setRequestQueryString ([("chat_id", user & Just . BC.pack . show . tChatID ), queryMsg $ user & tMessage ])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, either (\_ -> "/sendMessage") (\_ -> "/sendAnimation") (user & tMessage) ])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest
    where queryMsg (Left msg) = ("text", Just $ E.encodeUtf8 msg)
          queryMsg (Right msg) = ("animation", Just $ E.encodeUtf8 msg)

buildGetRequest :: Config -> Request
buildGetRequest cfg =
    setRequestHost (cfg & cBotHost)
  $ setRequestMethod (cfg & cMethod) 
  $ setRequestSecure (cfg & cSecure)
  $ setRequestQueryString ([("offset", Just $ cfg & cOffset), ("timeout", Just $ cfg & cTimeOut)])
  $ setRequestPath (mconcat[cfg & cApiPath, cfg & cToken, "/getUpdates"])
  $ setRequestPort (cfg & cPort)
  $ defaultRequest

makeResponse :: Config -> TParse -> TParse
makeResponse cfg user = case user & tMessage of
  Left "/help" -> user {tMessage = Left $ cfg & cTextMenuHelp}
  Left "/repeat" -> user {tMessage = Left $ cfg & cTextMenuRepeat}
  Left msg -> user
  Right msg -> user
-- Dopisat' tyt konady repeat, pochitat pro knopki i menu. K tomy ze zdes nado peredavat kolichestvo povtorov

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

  case mbMessage of
    Nothing -> do
       putStrLn "JSON is uncorrect"
       LC.putStrLn $ jsonBody
       L.writeFile "data.json" jsonBody
       loop cfg updateID
    Just message -> do
      putStrLn $ show message
      let updateID' = message & tUpdateID 
      if updateID == updateID' then loop cfg updateID'
      else do
        botResponse <- httpLBS (buildSendRequest cfg $ makeResponse cfg message) -- urlUpdate
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
  else ConsoleBot.greeting mConfig >> ConsoleBot.loop (mConfig & cRepeatCount)
