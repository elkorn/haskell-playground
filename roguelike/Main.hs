module Roguelike where

import Prelude hiding (Either(..))

import Level
import LevelGen
import Types
import UI.Terminal
import Utils

main :: IO ()
main = startGame

startGame :: IO ()
startGame = do
    blah
    world <- startingWorld
    prepareGame world
    gameLoop $ world

startingWorld :: IO WorldState
startingWorld = do
    customLevel <- generateLevel CaveGenerator $ LevelSpec Small
    return $ startingState
            { worldLevel = customLevel
            , worldLevels = [customLevel]
            , worldHero = startingHero
              {
                heroPosition = (1,1)
              , heroOldPosition = (1,1)
              }
            }


getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    ' ' -> return Wait
    'q' -> return Exit
    'r' -> return Restart
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
    Restart       -> startGame
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
