{-# LANGUAGE OverloadedStrings #-}

module Api.Core where

import Snap.Core
import Snap.Snaplet

import qualified Data.ByteString.Char8 as BS

data Api =
  Api

apiInit :: SnapletInit b Api
apiInit =
  makeSnaplet "api" "Core Api" Nothing $ do
  addRoutes apiRoutes
  return Api

apiRoutes :: [(BS.ByteString,Handler b Api ())]
apiRoutes = [("status",method GET respondOk)]

respondOk :: Handler b Api ()
respondOk =
  modifyResponse $
  setResponseCode 200
