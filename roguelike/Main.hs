module Roguelike where

import Prelude hiding (Either(..))

import Level
import LevelGen
import Types
import UI.Terminal
import Utils

main :: IO ()
main = do
    blah
    customLevel <- generateLevel $ LevelSpec Small
    let world = startingState
            { worldLevel = customLevel
            , worldLevels = [customLevel]
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
