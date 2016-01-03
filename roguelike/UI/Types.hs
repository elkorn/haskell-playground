module UI.Types where

-- class UI u where
--   prepareGame :: u -> IO ()
--   -- drawCharacter :: Coordinates -> IO ()
--   -- temporary, for convenience
--   drawCharacter :: u -> (Int, Int) -> IO ()
--   handleExit :: u -> IO ()

data UIMode = TerminalUI
            | DummyUI deriving (Read, Show)

