module Roguelike where

import Prelude hiding (Either(..))

import Types

import UI.Terminal

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

gameLoop :: WorldState -> IO ()
gameLoop world@(World hero _ _ _) = do
  drawCharacter $ heroPosition hero
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
handleDir world@(World hero@(Hero position _ _ _ oldPosition _ _) _ _ _) input = world
    { worldHero = hero
      { heroPosition = (clampCoordinatesToScreen $ position |+| dCoordinates)
      , heroOldPosition = oldPosition
      }
    }
    where dCoordinates = inputToCoordinates input

main :: IO ()
main = do
  prepareGame
  gameLoop $ startingState
