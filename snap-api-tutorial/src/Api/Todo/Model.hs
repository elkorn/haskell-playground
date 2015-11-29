module Api.Todo.Model where

import qualified Data.Text as T

data Todo =
  Todo {todoId :: Int
       ,todoText :: T.Text}
