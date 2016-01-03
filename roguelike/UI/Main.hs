module UI where

import System.Console.ANSI
import System.IO

import UI.Types
import UI.Dummy
import UI.Terminal

selectUi :: String -> Maybe UIMode
selectUi str = case readMaybe str of
  Just mode -> Just mode
  Nothing -> Nothing
