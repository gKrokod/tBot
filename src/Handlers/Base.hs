module Handlers.Base where 
import TData (ChatID, RepeatCount)
import Data.Function ((&))

data Handle m = Handle 
  {  findUser :: ChatID -> m (Maybe RepeatCount)
  ,  updateUser :: ChatID -> RepeatCount -> m ()
  ,  defaultRepeatCount :: RepeatCount
  }


giveRepeatCountFromBase :: (Monad m) => Handle m -> ChatID -> m (RepeatCount)
giveRepeatCountFromBase h user = do
  existUser <- findUser h user
  case existUser of
    Nothing -> do
      updateUser h user (h & defaultRepeatCount)
      giveRepeatCountFromBase h user
    Just repeatCount -> pure repeatCount


