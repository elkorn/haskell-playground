module LevelGen (generateLevel) where

import Control.Applicative

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (fromJust)

import Level
import System.Random (randomRIO, getStdGen, randomRs)
import Types
import Utils

type RoomLocationInGrid = (Int, Int)
type RectBoundaries = (Coordinates, Coordinates)
type RoomSize = (Int, Int)
type Size = (Int, Int)

data Wall
    = North
    | South
    | East
    | West
    deriving (Eq,Show)

data Room
    = Room { roomCoordinates :: RectBoundaries
           , roomMeldedWith :: S.Set Room
           , roomConnections :: S.Set Room
           }
    | Corridor { corridorCoordinates :: [RectBoundaries]
               , corridorGoesThroughRooms :: S.Set Room
               }
    deriving (Ord,Eq,Show)

roomLocation :: Room -> Coordinates
roomLocation = fst . roomCoordinates

minWidth = 4
minHeight = 4

corridorWidth = 2

generateLevel :: LevelSpec -> IO Level
generateLevel spec = do
    let baseLevel = emptyLevel
            { levelMax = (80, 40)
            }
    rooms <- generateRooms baseLevel [] 2
    corridors <- generateCorridors rooms
    print $ map corridorCoordinates corridors
    return $
        -- updateLevelMax $
        baseLevel
        { levelTiles = foldl M.union M.empty $
          map roomToTiles (rooms ++ corridors) 
        }

roomToTiles :: Room -> M.Map Coordinates Tile
roomToTiles (Room bounds@((startX, startY), (endX, endY)) meldedRooms _) =
  M.fromList tiles
  where
    tiles = zip coordinates $ map roomCoordinatesToTile coordinates
    coordinates = [(x,y) | x <- [startX .. endX], y <- [startY .. endY]]
    roomCoordinatesToTile :: Coordinates -> Tile
    roomCoordinatesToTile coordinates@(x, y)
      | shouldBeMelded coordinates = Floor
      | isBoundary x startX endX || isBoundary y startY endY = Wall
      | otherwise = Floor
    shouldBeMelded :: Coordinates -> Bool
    shouldBeMelded coordinates = any (flip isInsideBounds coordinates) meldBounds
    meldBounds = map roomCoordinates $ S.toList meldedRooms
    isBoundary :: Int -> Int -> Int -> Bool
    isBoundary coord startBound endBound = coord == startBound || coord == endBound
roomToTiles (Corridor legs crossedRooms) = let
  legRoomsWithoutIntersections = map legToRoom legs--foldl M.union M.empty (map (roomToTiles . legToRoom))
  legRoomsWithIntersections =
    map updateCrossIntersections legRoomsWithoutIntersections
  legToRoom :: RectBoundaries -> Room
  legToRoom leg = Room leg crossedRooms S.empty
  updateCrossIntersections :: Room -> Room
  updateCrossIntersections room =
    room {roomMeldedWith = S.union (roomMeldedWith room) $ S.fromList $ except room legRoomsWithoutIntersections }
  in foldl M.union M.empty $ map roomToTiles legRoomsWithIntersections

except :: (Eq a) => a -> [a] -> [a]
except val = filter (/=val)

generateRooms :: Level -> [Room] -> Int -> IO [Room]
generateRooms level existingRooms 0 = return existingRooms
generateRooms level existingRooms n = do
  let (maxX, maxY) = levelMax level
  room <- generateRoom level (maxX `div` 3, maxY `div` 3)
  (nextRooms, roomsLeft) <- setUpNextRooms room existingRooms n
  generateRooms level nextRooms roomsLeft
  where
    setUpNextRooms :: Room -> [Room] -> Int -> IO ([Room], Int)
    setUpNextRooms room existingRooms roomsLeft = do
        let intersectingRooms = filter (roomIntersects room) existingRooms
        let newRoomIntersectsExistingOnes = length intersectingRooms > 0
        if newRoomIntersectsExistingOnes
        then do
             -- shouldMeld <- coinFlip
             shouldMeld <- return False

             if shouldMeld
                then return (meldRooms room existingRooms intersectingRooms, n-1)
                else return (existingRooms, n)
        else return (room:existingRooms, n-1) 

generateRoom :: Level -> RoomSize -> IO Room
generateRoom level (maxWidth, maxHeight) = do
  let (maxX, maxY) = levelMax level
  width <- randomRIO (minWidth, maxWidth)
  height <- randomRIO (minHeight, maxHeight)
  startX <- randomRIO (0, maxX - width)
  startY <- randomRIO (0, maxY - height)
  return $ Room ((startX, startY), (startX + width, startY + height)) S.empty S.empty

intersects :: RectBoundaries -> RectBoundaries -> Bool
intersects ((x1a,y1a),(x2a,y2a)) ((x1b,y1b),(x2b,y2b)) =
  x1a <= x2b &&
  x2a >= x1b &&
  y1a <= y2b &&
  y2a >= y1b

roomIntersects :: Room -> Room -> Bool
roomIntersects (Room boundsA _ _) (Room boundsB _ _) =
  intersects boundsA boundsB
roomIntersects (Corridor legs _) (Room roomBounds _ _) =
  any (intersects roomBounds) legs
roomIntersects room@(Room _ _ _) corridor@(Corridor _ _) =
  roomIntersects corridor room

intersection :: RectBoundaries -> RectBoundaries -> RectBoundaries
intersection ((x1a,y1a),(x2a,y2a)) roomB =
  (minimum intersectingTiles, maximum intersectingTiles)
  where
    roomATiles = [(x,y) | x <- [x1a..x2a] , y <- [y1a..y2a]]
    intersectingTiles = filter (isInsideBounds roomB) roomATiles

isInsideBounds :: RectBoundaries -> Coordinates -> Bool
isInsideBounds ((roomX1, roomY1), (roomX2, roomY2)) (x,y) =
    x > roomX1 && x < roomX2 && y > roomY1 && y < roomY2

meldRoomWithAnother :: Room -> Room -> Room
meldRoomWithAnother roomA roomB =
  roomA {roomMeldedWith = S.insert roomB (roomMeldedWith roomA) }
  
meldRooms :: Room -> [Room] -> [Room] -> [Room]
meldRooms roomToMeldIn roomsToMeldInto intersectingRooms =
  (roomToMeldIn{roomMeldedWith = roomsMeldedIn}:roomsMeldedInto)
  where
    roomsMeldedIn = S.union (roomMeldedWith roomToMeldIn) (S.fromList intersectingRooms)
    roomsMeldedInto = meldNewRoomIntoExistingOnes roomToMeldIn intersectingRooms roomsToMeldInto
    meldNewRoomIntoExistingOnes :: Room -> [Room] -> [Room] -> [Room]
    meldNewRoomIntoExistingOnes roomToBeMelded intersectingRooms =
      map (meldRoomIfIntersects roomToBeMelded intersectingRooms)
    meldRoomIfIntersects :: Room -> [Room] -> Room -> Room
    meldRoomIfIntersects roomToBeMelded intersectingRooms roomToMeldInto 
      | roomToMeldInto `elem` intersectingRooms = meldRoomWithAnother roomToMeldInto roomToBeMelded
      | otherwise = roomToMeldInto

generateCorridors :: [Room] -> IO [Room]
generateCorridors rooms = do
  connections <- generateConnections rooms
  return $ map updateCrossedRooms $ createCorridorsForConnections connections
  where
    generateConnections :: [Room] -> IO [(Room, Room)]
    generateConnections rooms = do
      let n = length rooms
      numberOfConnections <- randomRIO (n, n)
      let allConnections = (,) <$> rooms <*> rooms
      selectRandom numberOfConnections allConnections
    selectRandom :: Int -> [a] -> IO [a]
    selectRandom howMany items = do
        stdGen <- getStdGen
        let indices = randomRs (0, (length items) - 1) stdGen
        return $ map (items !!) (take howMany indices)
    createCorridorsForConnections :: [(Room, Room)] -> [Room]
    createCorridorsForConnections connections = foldl (\result connection -> (createCorridor connection):result) [] connections
    updateCrossedRooms :: Room -> Room
    updateCrossedRooms corridor@(Corridor legs crossedRooms) =
      corridor {corridorGoesThroughRooms = S.union crossedRooms (S.fromList $ filter (roomIntersects corridor) rooms)}

createCorridor :: (Room, Room) -> Room
createCorridor (Room roomACoords@((x1a, y1a), (x2a, y2a)) _ _, Room roomBCoords@((x1b, y1b), (x2b, y2b)) _ _) = let
    -- The horizontal and vertical legs have to be created differently. 
    -- (startX, endX) = getInner (x1a, x2a) (x1b, x2b)
    -- (startY, endY) = getInner (y1a, y2a) (y1b, y2b)
    -- To make the corridor enter the start and finish room, discern between
    -- horizontal and vertical legs and grow only the main dimension.
    -- in Corridor [((startX, startY), (endX, startY + corridorWidth)),((startX, startY), (startX + corridorWidth, endY))] S.empty
    (startX, startY) = getCenter roomACoords
    (endX, endY) = getCenter roomBCoords
    halfWidth = corridorWidth `div` 2
    in Corridor [grow level1 1 ((startX, startY - 1), (endX, startY + 1)), grow level1 1 ((endX - 1, startY), (endX + 1, endY))] S.empty

getInner :: (Int, Int) -> (Int, Int) -> (Int, Int) -- These are not correct coordinates, the format is (x,x) or (y,y).
getInner leftValues rightValues = let
    allValues = leftValues `cross` rightValues
    differences = map (\(a,b) -> a - b) allValues
    absDifferences = map abs differences
    minAbsDifference = minimum absDifferences
    minDifference = minimum differences
    minIndex = fromJust $ L.elemIndex minAbsDifference absDifferences
    min@(minA, minB) = allValues !! minIndex
    in if minA < minB then min
                    else (minB, minA)
cross :: (a, a) -> (a, a) -> [(a,a)]
(a1, a2) `cross` (a3, a4) = (,) <$> [a1,a2] <*> [a3,a4]

generateRoomAdjacency :: [Room] -> Level -> M.Map Room [Room]
generateRoomAdjacency rooms level =
  M.fromList $ zip rooms $ map getAdjacents rooms
  where
    getAdjacents :: Room -> [Room]
    getAdjacents room = filter (isAdjacent room) rooms
    isAdjacent :: Room -> Room -> Bool
    isAdjacent roomA roomB =
      roomA /= roomB &&
      intersects intersectionRangeA intersectionRangeB
      where
        intersectionRangeA = grow 1 $ roomCoordinates roomA
        intersectionRangeB = grow 1 $ roomCoordinates roomB
    grow :: Int -> RectBoundaries -> RectBoundaries
    grow howMuch (start, end) = let
        dSize = (howMuch, howMuch)
        clampToLevel = flip clampCoordinatesToLevel $ level
        in (clampToLevel $ start |-| dSize, clampToLevel $ end |+| dSize)
