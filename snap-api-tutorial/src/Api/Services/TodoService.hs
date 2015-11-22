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
       (pgsInit, HasPostgres(..), Postgres(..), query_)

data TodoService =
  TodoService {_pg :: Snaplet Postgres}

makeLenses ''TodoService

todoRoutes :: [Route b TodoService ()]
todoRoutes = [("/",method GET getTodos)]

getTodos :: Handler b TodoService ()
getTodos =
  do todos <- query_ "SELECT * FROM todos"
     modifyResponse $
       setHeader "Content-Type" "application/json"
     writeLBS . encode $
       (todos :: [Todo])

todoServiceInit :: SnapletInit b TodoService
todoServiceInit =
  makeSnaplet "todos" "Todo Service" Nothing $
  do postgres <- nestSnaplet "pg" pg pgsInit
     addRoutes todoRoutes
     return $
       TodoService postgres

instance HasPostgres (Handler b TodoService) where
  getPostgresState = with pg get
