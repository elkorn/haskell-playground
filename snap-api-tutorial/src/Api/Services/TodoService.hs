{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.TodoService where

import Api.Types (Todo(Todo), Route)

import Control.Lens (makeLenses)
import Control.Monad.State.Class (get)
import Data.Aeson (encode)

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
       (pgsInit, HasPostgres(..), Postgres(..), query_, execute, Only(..))

data TodoService =
  TodoService {_pg :: Snaplet Postgres}

type TodoServiceHandler base = Handler base TodoService ()

makeLenses ''TodoService

todoRoutes :: [Route b TodoService ()]
todoRoutes =
  [("/",method GET getTodos),("/",method POST createTodo)]

getTodos :: TodoServiceHandler b
getTodos =
  do todos <- query_ "SELECT * FROM todos"
     modifyResponse $
       setHeader "Content-Type" "application/json"
     writeLBS . encode $
       (todos :: [Todo])

createTodo :: TodoServiceHandler b
createTodo =
  do todoTextParam <- getPostParam "text"
     newTodo <-
       execute "INSERT INTO todos (text) VALUES (?)" (Only todoTextParam)
     let responseCode =
           if (newTodo == 1)
              then 201
              else 500
     modifyResponse $ setResponseCode responseCode

todoServiceInit :: SnapletInit b TodoService
todoServiceInit =
  makeSnaplet "todos" "Todo Service" Nothing $
  do postgres <- nestSnaplet "pg" pg pgsInit
     addRoutes todoRoutes
     return $
       TodoService postgres

instance HasPostgres (Handler b TodoService) where
  getPostgresState = with pg get
