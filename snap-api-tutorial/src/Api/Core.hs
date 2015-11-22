{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Core where

import Control.Lens (makeLenses)
import qualified Data.ByteString.Char8 as BS

import Snap.Core
import Snap.Snaplet

import Api.Services.TodoService
       (TodoService(TodoService), todoServiceInit)

data Api =
  Api {_todoService :: Snaplet TodoService}

-- The `''` operator takes the `App` type constructor's name.
makeLenses ''Api

apiInit :: SnapletInit b Api
apiInit =
  makeSnaplet "api" "Core Api" Nothing $
  do ts <- nestSnaplet "todos" todoService todoServiceInit
     addRoutes apiRoutes
     return $ Api ts

apiRoutes :: [(BS.ByteString,Handler b Api ())]
apiRoutes = [("status",method GET respondOk)]

respondOk :: Handler b Api ()
respondOk =
  modifyResponse $
  setResponseCode 200
