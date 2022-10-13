module Main (main) where
import Lib
-- import Data.Either --fromLeft
import TData
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

buildSendRequest :: TParse -> Request
buildSendRequest user =
    setRequestHost botHost
  $ setRequestMethod "GET" 
  $ setRequestSecure True
  $ setRequestQueryString ([("chat_id", (Just . BC.pack . show . tChatID) user), queryMsg $ tMessage user ])
  $ setRequestPath (mconcat[apiPath, myToken, either (\_ -> "/sendMessage") (\_ -> "/sendAnimation") (tMessage user)])
  $ setRequestPort 443
  $ defaultRequest
    where queryMsg (Left msg) = ("text", Just $ E.encodeUtf8 msg)
          queryMsg (Right msg) = ("animation", Just $ E.encodeUtf8 msg)

buildGetRequest :: Request
buildGetRequest =
    setRequestHost botHost
  $ setRequestMethod "GET" 
  $ setRequestSecure True
  $ setRequestQueryString ([("offset", myOffset), ("timeout", myTimeOut)])
  $ setRequestPath (mconcat[apiPath, myToken, "/getUpdates"])
  $ setRequestPort 443
  $ defaultRequest

loop :: UpdateID -> IO()
loop updateID = do
  response <- httpLBS buildGetRequest 
  putStrLn $ "Get . The status code was: " ++
       show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  let jsonBody = getResponseBody (response)
  -- LC.putStrLn $ jsonBody
  -- L.writeFile "data.json" jsonBody
  let result = decode jsonBody :: Maybe TParse

  case result of
    Nothing -> do
       putStrLn "JSON is uncorrect"
       LC.putStrLn $ jsonBody
       L.writeFile "data.json" jsonBody
       loop updateID
    Just answer -> do
      putStrLn $ show answer
      let updateID' = tUpdateID answer
      if updateID == updateID' then loop updateID'
      else do
        response <- httpLBS (buildSendRequest answer) -- urlUpdate
        putStrLn $ "After SEnd The status code was: " ++
           show (getResponseStatusCode response)
        print $ getResponseHeader "Content-Type" response
        LC.putStrLn $ getResponseBody response
        loop updateID'



main :: IO ()
main = loop 0 
