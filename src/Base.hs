module Base where
import TData (UserDB, ChatID, RepeatCount, Config(..)) -- УБрать отсюда userDb в идеале и определять его в этом модуле, для этого
-- переделать хендлер для общей программы
import qualified Handlers.Base as Handler
import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromMaybe)
-- import Data.Bool (bool)
import Data.Function ((&))
import TData (UserDB, ChatID, RepeatCount)
import Control.Concurrent

newtype UserDataBase = UserDataBase (MVar UserDB)

newBase :: IO UserDataBase
newBase = do
  m <- newMVar Map.empty
  return $ UserDataBase m

insert :: UserDataBase -> ChatID -> RepeatCount -> IO ()
insert (UserDataBase m) user count = do
  base <- takeMVar m
  let base' = Map.insert user count base
  putMVar m base'
  seq base' (return ())

lookup :: UserDataBase -> ChatID -> IO (Maybe RepeatCount)
lookup (UserDataBase m) user = do
  base <- takeMVar m
  putMVar m base
  return $ Map.lookup user base


-- new :: Config -> Handler.Handle IO 
-- new cfg = Handler.Handle {
--                            Handler.defaultRepeatCount = cfg & cRepeatCount  -- e.g. 10
-- 			 , Handler.findUser = \_ -> pure $ Just $ cfg & cRepeatCount
-- 			 , Handler.updateUser = \_ _ -> pure ()
-- 			 }

close :: Handler.Handle IO -> ()
close = undefined


