module Api.Todo.Storage where

import Snap.Snaplet.PostgresqlSimple

import Api.Todo.Model (Todo(..))

instance FromRow Todo where
  fromRow = Todo <$> field <*> field

