{-# LANGUAGE OverloadedStrings #-}

module Api.Status.Service where

import Snap.Core
import Snap.Snaplet

import Api.Types (Route)

data StatusService =
  StatusService

type StatusServiceHandler base = Handler base StatusService ()

showStatus :: StatusServiceHandler b
showStatus = do
    modifyResponse $ setResponseCode 500
    writeLBS "All is well."

statusRoutes :: [Route b StatusService ()]
statusRoutes = [("/",method GET showStatus)]

statusServiceInit :: SnapletInit b StatusService
statusServiceInit =
  makeSnaplet "status" "Status Service" Nothing $
  do
    addRoutes statusRoutes
    return StatusService
