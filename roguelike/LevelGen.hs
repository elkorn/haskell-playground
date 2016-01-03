module LevelGen where

import qualified Data.Set as S
import qualified Data.Map as M

import Level
import System.Random (randomRIO)
import Types

type RoomLocationInGrid = (Int, Int)
type RectBoundaries = (Coordinates, Coordinates)
type RoomSize = (Int, Int)
type Size = (Int, Int)

data Room = Room
    { roomCoordinates :: RectBoundaries
    -- , roomLocationInGrid :: RoomLocationInGrid
    , roomConnections :: S.Set RoomLocationInGrid
    } deriving Show

minWidth = 0
minHeight = 0

generateRoom :: Level -> RoomSize -> IO Room
generateRoom level (maxWidth, maxHeight) = do
  let (maxX, maxY) = levelMax level
  width <- randomRIO (3, maxWidth)
  height <- randomRIO (3, maxHeight)
  startX <- randomRIO (0, maxX - width)
  startY <- randomRIO (0, maxY - height)
  return $ Room ((startX, startY), (startX + width, startY + height)) S.empty

intersects :: RectBoundaries -> RectBoundaries -> Bool
intersects ((x1a,y1a),(x2a,y2a)) ((x1b,y1b),(x2b,y2b)) =
  x1a <= x2b &&
  x2a >= x1b &&
  y1a <= y2b &&
  y2a >= y1b

generateRooms :: Level -> [Room] -> Int -> IO [Room]
generateRooms level existingRooms 0 = return existingRooms
generateRooms level existingRooms n = do
  let (maxX, maxY) = levelMax level
  room <- generateRoom level (maxX `div` 3, maxY `div` 3)
  if any (intersects $ roomCoordinates room) $ map roomCoordinates existingRooms
    then generateRooms level existingRooms n
    else generateRooms level (room:existingRooms) (n-1)

generateLevel :: LevelSpec -> IO Level
generateLevel spec = do
    let baseLevel = emptyLevel
            { levelMax = (80, 40)
            }
    rooms <- generateRooms baseLevel [] 5
    return $
        -- updateLevelMax $
        baseLevel
        { levelTiles = foldl M.union M.empty $
          map roomToTiles rooms
        }

roomToTiles :: Room -> M.Map Coordinates Tile
roomToTiles (Room ((startX, startY), (endX, endY)) _) =
  M.fromList tiles
  where
    tiles = zip coordinates $ map roomCoordinatesToTile coordinates
    coordinates = [(x,y) | x <- [startX .. endX], y <- [startY .. endY]]
    isBoundary coord startBound endBound = coord == startBound || coord == endBound
    roomCoordinatesToTile :: Coordinates -> Tile
    roomCoordinatesToTile (x, y) =
      if isBoundary x startX endX || isBoundary y startY endY
        then Wall
        else Floor