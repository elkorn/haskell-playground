module Roguelike where

import Prelude hiding (Either(..))

import Types
import Level

import UI.Terminal

main :: IO ()
main = do
    let world = startingState
            { worldLevel = level1
            , worldLevels = [level1]
            , worldHero = startingHero
              {
                heroPosition = (1,1)
              , heroOldPosition = (1,1)
              }
            }
    prepareGame world
    gameLoop $ world

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    ' ' -> return Wait
    'q' -> return Exit
    'w' -> return $ Dir Up
    's' -> return $ Dir Down
    'a' -> return $ Dir Left
    'd' -> return $ Dir Right
    _ -> getInput

gameLoop :: WorldState -> IO ()
gameLoop world@(World hero _ _ _) = do
  drawHero $ world
  input <- getInput
  case input of
    Dir direction -> gameLoop $ handleDirection world direction
    Wait          -> gameLoop world
    Exit          -> handleExit

directionToCoordinates :: Direction -> Coordinates
directionToCoordinates Left = (-1, 0)
directionToCoordinates Right  = (1, 0)
directionToCoordinates Up  = (0, -1)
directionToCoordinates Down = (0, 1)

(|+|) :: Coordinates -> Coordinates -> Coordinates
(x1, y1) |+| (x2, y2) = (x1+x2,y1+y2)

clamp :: Ord a => a -> a -> a -> a
clamp val _min _max = max _min $ min _max val

-- TODO: read screen size from config
clampCoordinatesToLevel :: Coordinates -> Level -> Coordinates
clampCoordinatesToLevel (oldX,oldY) (Level _ _ _ _ (maxX, maxY) _ _)= let
  newX = clamp oldX 0 maxX
  newY = clamp oldY 0 maxY
  in (newX, newY)

handleDirection :: WorldState -> Direction -> WorldState
handleDirection world@(World hero@(Hero position _ _ _ oldPosition _ _) _ level _) direction =
  if isWall newPosition level
        then world
        else world
            { worldHero = hero
                { heroPosition = (clampCoordinatesToLevel newPosition level)
                , heroOldPosition = position
                }
            }
  where dCoordinates = directionToCoordinates direction
        newPosition = position |+| dCoordinates
