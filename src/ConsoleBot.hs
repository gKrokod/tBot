module ConsoleBot
    (loop, greeting
    ) where
import Data.Char
import TData (Config (..))
import Data.Function ((&))
import Data.Text as T
import Data.Text.IO as TIO

nameBot :: T.Text
nameBot = "Echo-bot: "
nameYou :: T.Text
nameYou = "You: "
repeatMsg :: T.Text
repeatMsg = "Please, type a new value = "
repeatErrorMsg :: T.Text
repeatErrorMsg = "Incorrect value. Please Try Again."


replyConsoleBot :: T.Text -> T.Text
replyConsoleBot message = mconcat [nameBot, message]

responceConsoleBot :: (T.Text -> IO ()) -> T.Text -> IO ()
responceConsoleBot f = f . replyConsoleBot

answer :: Config -> T.Text -> IO (Config)
answer cfg "/help" = do
  responceConsoleBot TIO.putStrLn (cfg & cTextMenuHelp)
  return cfg
answer cfg "/repeat" = do
  responceConsoleBot TIO.putStrLn (mconcat [cfg & cTextMenuRepeat, T.pack $ show $ cfg & cRepeatCount])
  responceConsoleBot TIO.putStrLn repeatMsg
  repeatCount <- TIO.getLine
  if not (T.null repeatCount) && (T.all isDigit repeatCount)
  then return cfg {
                    cRepeatCount = read $ T.unpack repeatCount
                  }
  else do
    responceConsoleBot TIO.putStrLn repeatErrorMsg
    answer cfg "/repeat"
answer cfg msg = do
  let echoMessage = Prelude.replicate (fromIntegral $ cfg & cRepeatCount) msg
  mapM_ (responceConsoleBot TIO.putStrLn) echoMessage
  return cfg 

greeting :: Config -> IO ()
greeting cfg = responceConsoleBot TIO.putStrLn (cfg & cTextMenuHelp) >> loop cfg

loop :: Config -> IO ()
loop cfg = do
  TIO.putStr nameYou 
  msg <- TIO.getLine
  cfg' <- answer cfg msg
  loop cfg'
