module Debug where

import Control.Monad
import qualified Data.Map as M
import System.Console.ANSI
import System.IO

import Types

printTiles :: M.Map Coordinates Tile -> IO ()
printTiles tiles = do
  clearScreen
  forM_ (M.toList tiles) printTile
  putStrLn ""
  where printTile :: (Coordinates, Tile) -> IO ()
        printTile ((x,y), tile) = do
          setCursorPosition y x
          case tile of
            Wall -> putChar '#'
            Floor -> putChar ' '
