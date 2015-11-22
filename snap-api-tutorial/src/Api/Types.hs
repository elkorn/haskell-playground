{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import Control.Applicative
import Data.Aeson (ToJSON(toJSON), object, (.=))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data Todo =
  Todo {todoId :: Int
       ,todoText :: T.Text}

instance FromRow Todo where
  fromRow = Todo <$> field <*> field

-- note that the data defined in record syntax also works as a literal pattern match.
instance ToJSON Todo where
  toJSON (Todo theId theText) =
    object ["id" .= theId,"text" .= theText]

type Route baseStateType serviceType serviceArgType = (B.ByteString, (Handler baseStateType serviceType serviceArgType))
