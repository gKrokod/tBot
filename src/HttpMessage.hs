module HttpMessage (buildGetRequest, buildSendRequest, buildCallBackQuery, makeResponse) where
import TData (Config(..), QueryID, TParse(..))
import qualified Data.Text.Encoding as E (encodeUtf8)
import Network.HTTP.Simple --(parseRequest, Request, httpLBS, getResponseBody, getResponseStatusCode, getResponseHeader)
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Function ((&))
import Parse (justKeyBoard)

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
  Left "/repeat" -> user {tMessage = Left $ cfg & cTextMenuRepeat, keyboardMenu = justKeyBoard, isCommand = True}
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
