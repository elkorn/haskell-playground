{-# LANGUAGE DataKinds #-}
module UI.Dummy where

import UI.Types

instance UI DummyUI where
  prepareGame _ = do
    putStrLn $ "Preparing game"
  drawCharacter _ coordinates = do
    putStrLn $ "Drawing character at " ++ show coordinates
  handleExit _ = do
    putStrLn "Handling exit"
