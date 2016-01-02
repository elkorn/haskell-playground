module Roguelike where

import Prelude hiding (Either(..))
import System.Console.ANSI

import System.IO

import Types

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput

drawHero :: Coordinates -> IO ()
drawHero (x, y) = do
  clearScreen
  setCursorPosition y x
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"

gameLoop :: WorldState -> IO ()
gameLoop world@(World hero _ _ _) = do
  drawHero $ heroPosition hero
  input <- getInput
  case input of
    Exit ->  handleExit
    _    -> gameLoop $ handleDir world input

inputToCoordinates :: Input -> Coordinates
inputToCoordinates Left = (-1, 0)
inputToCoordinates Right  = (1, 0)
inputToCoordinates Up  = (0, -1)
inputToCoordinates Down = (0, 1)
inputToCoordinates _ = (0, 0)

(|+|) :: Coordinates -> Coordinates -> Coordinates
(x1, y1) |+| (x2, y2) = (x1+x2,y1+y2)

clamp :: Ord a => a -> a -> a -> a
clamp val _min _max = max _min $ min _max val

-- TODO: read screen size from config
clampCoordinatesToScreen :: Coordinates -> Coordinates
clampCoordinatesToScreen (oldX,oldY) = let _min = 1
                                           _max = 81
                                           newX = clamp oldX _min _max
                                           newY = clamp oldY _min _max
    in (newX, newY)

handleDir :: WorldState -> Input -> WorldState
handleDir (World (Hero position gold hp items oldPosition wield wears) depth level levels) input = World
        (Hero (clampCoordinatesToScreen $ position |+| dCoordinates) gold hp items position wield wears)
        depth
        level
        levels
    where dCoordinates = inputToCoordinates input
handleExit :: IO ()
handleExit = do
  setSGR [ Reset ]
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Goodbye!"

hero :: Hero
hero = Hero (1,1) 0 10 [] (1,1) (Weapon 0 "" 0) (Armor 0 "")

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Game!"
  gameLoop $ World hero 0 0 []
