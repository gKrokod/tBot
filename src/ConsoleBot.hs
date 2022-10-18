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


replyConsoleBot :: T.Text -> T.Text
replyConsoleBot message = mconcat [nameBot, message]

responceConsoleBot :: (T.Text -> IO ()) -> T.Text -> IO ()
responceConsoleBot f = f . replyConsoleBot

answer :: Integer -> T.Text -> IO (Integer)
answer n "/help" = do
  responceConsoleBot TIO.putStrLn "Welcome! I am console echo-bot."
  responceConsoleBot TIO.putStrLn "Possible command : /help, /repeat"
  responceConsoleBot TIO.putStrLn "WHat about me? Good to meet you!" 
  return n
answer n "/repeat" = do
  responceConsoleBot TIO.putStrLn (mconcat ["Number of repeat = ",T.pack $ show n])
  responceConsoleBot TIO.putStrLn "Please, type a new value = "
  n' <- Prelude.getLine
  if not (Prelude.null n') && (Prelude.all isDigit n')
  then return $ read n'
  else do
    responceConsoleBot TIO.putStrLn "Incorrect value. Please Try Again."
    answer n "/repeat"
answer n msg = do
  let echoMessage = T.replicate (fromIntegral n) msg
  responceConsoleBot TIO.putStrLn echoMessage
  return n

greeting :: Config -> IO ()
greeting cfg = do
  responceConsoleBot TIO.putStrLn (cfg & cTextMenuHelp)
  -- responceConsoleBot putStrLn "Possible command : /help, /repeat"

loop :: Integer -> IO ()
loop n = do
  TIO.putStrLn "You: "
  msg <- TIO.getLine
  n' <- answer n msg
  loop n'
