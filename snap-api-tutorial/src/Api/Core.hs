{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Core where

import Control.Lens (makeLenses)
import Snap.Snaplet

import Api.Todo.Service
       (TodoService, todoServiceInit)
import Api.Status.Service (StatusService, statusServiceInit)

data Api =
  Api {_todoService :: Snaplet TodoService
      ,_statusService :: Snaplet StatusService}


-- The `''` operator takes the `App` type constructor's name.
makeLenses ''Api

apiInit :: SnapletInit b Api
apiInit =
  makeSnaplet "api" "Core Api" Nothing $
  do ts <- nestSnaplet "todos" todoService todoServiceInit
     ss <- nestSnaplet "status" statusService statusServiceInit
     return $ Api ts ss
