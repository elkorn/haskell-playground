{-# LANGUAGE OverloadedStrings #-}

module Api.Todo.Serialization where


import Data.Aeson (ToJSON(toJSON), object, (.=))

import Api.Todo.Model (Todo(..))

-- note that the data defined in record syntax also works as a literal pattern match.
instance ToJSON Todo where
  toJSON (Todo theId theText) =
    object ["id" .= theId,"text" .= theText]
