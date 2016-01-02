module UI where

import System.Console.ANSI
import System.IO

import UI.Types
import UI.Dummy
import UI.Terminal

data UIMode = Console
            | Dummy deriving Show

-- problem :(
-- selectUi :: (UI u) => UIMode -> u
-- selectUi mode = case mode of
--   Console -> TerminalUI
--   Dummy -> DummyUI
