module UI.Terminal where

import System.Console.ANSI
import System.IO

-- import UI.Types

-- instance UI TerminalUI where
--   prepareGame _ = do
--     hSetEcho stdin False
--     hSetBuffering stdin NoBuffering
--     hSetBuffering stdout NoBuffering
--     hideCursor
--     setTitle "Game!"
--   drawCharacter _ (x, y) = do
--     clearScreen
--     setCursorPosition y x
--     setSGR [ SetConsoleIntensity BoldIntensity
--             , SetColor Foreground Vivid Blue ]
--     putStr "@"
--   handleExit _ = do
--     setSGR [ Reset ]
--     clearScreen
--     setCursorPosition 0 0
--     showCursor
--     putStrLn "Goodbye!"

prepareGame :: IO ()
prepareGame = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Game!"

drawCharacter :: (Int, Int) -> IO ()
drawCharacter (x, y) = do
  clearScreen
  setCursorPosition y x
  setSGR [ SetConsoleIntensity BoldIntensity
          , SetColor Foreground Vivid Blue ]
  putStr "@"

handleExit :: IO ()
handleExit = do
  setSGR [ Reset ]
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Goodbye!"
