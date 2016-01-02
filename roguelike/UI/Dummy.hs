module UI.Dummy where

import UI.Types

data DummyUI = DummyUI

instance UI DummyUI where
  prepareGame _ = do
    putStrLn $ "Preparing game"
  drawCharacter _ coordinates = do
    putStrLn $ "Drawing character at " ++ show coordinates
  handleExit _ = do
    putStrLn "Handling exit"
