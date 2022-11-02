module Handlers.Base where 
import TData (ChatID, RepeatCount)

data Handle a = Handle 
  {  loadRepeatCount :: ChatID -> RepeatCount
  ,  updateDB :: ChatID -> RepeatCount -> Handle a
  ,  defaultRepeatCount :: RepeatCount
  ,  userDB :: a
  }
