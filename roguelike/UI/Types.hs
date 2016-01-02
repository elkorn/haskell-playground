module UI.Types (UI, prepareGame, drawCharacter, handleExit) where

class UI u where
  prepareGame :: u -> IO ()
  -- drawCharacter :: Coordinates -> IO ()
  -- temporary, for convenience
  drawCharacter :: u -> (Int, Int) -> IO ()
  handleExit :: u -> IO ()
