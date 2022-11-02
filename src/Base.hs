module Base where
import TData (UserDB, ChatID, RepeatCount, Config(..)) -- УБрать отсюда userDb в идеале и определять его в этом модуле, для этого
-- переделать хендлер для общей программы
import qualified Handlers.Base as Handler
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.Function ((&))

-- data Handle a = Handle -- вот такой Хендлер я сделал для базы данных пользователей, чтобы отработать логику
--   {  loadRepeatCount :: ChatID -> RepeatCount
--   ,  updateDB :: ChatID -> RepeatCount -> Handle a
--   ,  defaultRepeatCount :: RepeatCount
--   ,  userDB :: a
--   }
--
-- type ChatID = Integer
-- type RepeatCount = Int

-- type UserDB = Map.Map ChatID RepeatCount
--o
-------------------Здесь я пробую описать конкретную реализацию для базы данных пользователей на основе Data.Map
loadRepeatCount :: Handler.Handle UserDB -> ChatID -> RepeatCount
loadRepeatCount h user = fromMaybe configValue (Map.lookup user base)
  where base = h & Handler.userDB
        configValue = h & Handler.defaultRepeatCount

updateDB :: Handler.Handle UserDB -> ChatID -> RepeatCount -> Handler.Handle UserDB 
updateDB h user count = h { Handler.userDB = newBase }
  where newBase = bool (Map.insert user count base) base userIsMember
        base = h & Handler.userDB
	userIsMember = Map.member user base

---------- здесь я пытаюсь создать свой экземпляр базы данных с "методоами" для работы с учетром настроек из файла (config)--
new :: Config -> Handler.Handle UserDB
-----------первый два поля принимаются и работают без ошибок. Третье проходит, но не работает. Четвертое не проходит.
new cfg = Handler.Handle {
                           Handler.defaultRepeatCount = cfg & cRepeatCount  -- e.g. 10
			 , Handler.userDB = Map.empty  
--------здесь компилятор выводит ошибку, что слишком много аргументов у функции, ведь я еще обрабатываю Handler.Handle
--но как его передать при инициализаии не понимаю
                         , Handler.loadRepeatCount = \a -> loadRepeatCount undefined a -- так принимает, но не работает
                         , Handler.updateDB = updateDB  -- так не принимает компилятор
			 }

close :: Handler.Handle UserDB -> ()
close = undefined


