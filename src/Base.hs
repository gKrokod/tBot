module Base where
import TData (UserDB, ChatID, RepeatCount, Config(..)) -- УБрать отсюда userDb в идеале и определять его в этом модуле, для этого
-- переделать хендлер для общей программы
import qualified Handlers.Base as Handler
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.Function ((&))

-- data Handle m = Handle 
--   {  findUser :: ChatID -> m (Maybe User)
--   ,  updateUser :: ChatID -> RepeatCount -> m ()
--   ,  defaultRepeatCount :: RepeatCount
--   }
--
-- type ChatID = Integer
-- type RepeatCount = Int

-- type UserDB = Map.Map ChatID RepeatCount
--o
-------------------Здесь я пробую описать конкретную реализацию для базы данных пользователей на основе Data.Map
-- loadRepeatCount :: Handler.Handle UserDB -> ChatID -> RepeatCount
-- loadRepeatCount h user = fromMaybe configValue (Map.lookup user base)
--   where base = h & Handler.userDB
--         configValue = h & Handler.defaultRepeatCount
--
-- updateDB :: Handler.Handle UserDB -> ChatID -> RepeatCount -> Handler.Handle UserDB 
-- updateDB h user count = h { Handler.userDB = newBase }
--   where newBase = bool (Map.insert user count base) base userIsMember
--         base = h & Handler.userDB
-- 	userIsMember = Map.member user base

new :: Config -> Handler.Handle IO 
new cfg = Handler.Handle {
                           Handler.defaultRepeatCount = cfg & cRepeatCount  -- e.g. 10
			 , Handler.findUser = \_ -> pure $ Just $ cfg & cRepeatCount
			 , Handler.updateUser = \_ _ -> pure ()
			 }

close :: Handler.Handle IO -> ()
close = undefined


