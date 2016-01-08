module LevelGen.Default where

import Control.Applicative
import Control.Monad
import Debug.Trace

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

data Room
    = Room { roomCoordinates :: RectBoundaries
           , roomMeldedWith :: S.Set Room
           , roomConnections :: S.Set Room
           }
    | Corridor { corridorCoordinates :: [RectBoundaries]
               , corridorGoesThroughRooms :: S.Set Room
               , corridorCrossesCorridors :: S.Set Room
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
    rooms <- generateRooms baseLevel [] 4
    -- print $ map roomCoordinates rooms
    -- print $ map (getCenter . roomCoordinates) rooms
    corridors <- generateCorridors rooms
    -- print $ map corridorCoordinates corridors
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
    coordinates = rectCoordinates bounds
    roomCoordinatesToTile :: Coordinates -> Tile
    roomCoordinatesToTile coordinates@(x, y)
      | shouldBeMelded coordinates = Floor
      | isBoundary x startX endX || isBoundary y startY endY = Wall
      | otherwise = Floor
    shouldBeMelded :: Coordinates -> Bool
    shouldBeMelded coordinates = any (flip isInsideBounds coordinates) meldBounds
    meldBounds = map roomCoordinates $ S.toList meldedRooms
roomToTiles (Corridor legs crossedRooms crossedCorridors) = let
  legToTiles :: RectBoundaries -> M.Map Coordinates Tile
  legToTiles leg@((startX, startY), (endX, endY)) =
    M.fromList $ zip coordinates $ map roomCoordinatesToTile coordinates
    where coordinates = rectCoordinates leg
          roomCoordinatesToTile coordinates@(x, y)
           | shouldBeMelded coordinates = Floor
           | isBoundary x startX endX || isBoundary y startY endY = Wall
           | otherwise = Floor
          shouldBeMelded :: Coordinates -> Bool
          shouldBeMelded coordinates = any (flip isInsideBounds coordinates) meldBounds
          meldBounds = ((map roomCoordinates $ S.toList crossedRooms) ++
                        (concat $ map corridorCoordinates $ S.toList crossedCorridors) ++
                        (except leg legs))
  in foldl M.union M.empty (map legToTiles legs)

isBoundary :: Int -> Int -> Int -> Bool
isBoundary coord startBound endBound = coord == startBound || coord == endBound

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
intersects a@((x1a,y1a),(x2a,y2a)) b@((x1b,y1b),(x2b,y2b)) =
   x1a <= x2b &&
   x2a >= x1b &&
   y1a <= y2b &&
   y2a >= y1b

roomIntersects :: Room -> Room -> Bool
roomIntersects (Room boundsA _ _) (Room boundsB _ _) =
  intersects boundsA boundsB
roomIntersects (Corridor legs _ _) (Room roomBounds _ _) =
  any (intersects roomBounds) legs
roomIntersects room@(Room _ _ _) corridor@(Corridor _ _ _) =
  roomIntersects corridor room
roomIntersects (Corridor legsA _ _) corridorB@(Corridor _ _ _) =
  any (legIntersects corridorB) legsA
  where legIntersects :: Room -> RectBoundaries -> Bool
        legIntersects corridor leg =
          roomIntersects corridor (Room leg S.empty S.empty)

rectCoordinates :: RectBoundaries -> [Coordinates]
rectCoordinates ((x1a,y1a),(x2a,y2a)) =  [(x,y) | x <- [minX..maxX] , y <- [minY..maxY]]
  where
    [minX, maxX] = L.sort [x1a, x2a]
    [minY, maxY] = L.sort [y1a, y2a]

intersection :: RectBoundaries -> RectBoundaries -> RectBoundaries
intersection roomA@((x1a,y1a),(x2a,y2a)) roomB =
  (minimum intersectingCoordinates, maximum intersectingCoordinates)
  where
    roomACoordinates = rectCoordinates roomA
    intersectingCoordinates = filter (isInsideBounds roomB) roomACoordinates

isInsideBounds :: RectBoundaries -> Coordinates -> Bool
isInsideBounds ((boundsX1, boundsY1), (boundsX2, boundsY2)) (x,y) =
    x > minX && x < maxX && y > minY && y < maxY
    where minX = minimum [boundsX1, boundsX2]
          maxX = maximum [boundsX1, boundsX2]
          minY = minimum [boundsY1, boundsY2]
          maxY = maximum [boundsY1, boundsY2]

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
  -- TODO: update corridor intersections
  let corridors = createCorridorsForConnections connections
  return $ map ((updateCrossedCorridors corridors) . (updateCrossedRooms rooms)) corridors
  where
    generateConnections :: [Room] -> IO [(Room, Room)]
    generateConnections rooms = do
      let n = length rooms
      numberOfConnections <- randomRIO (n `div` 2, 2*n)
      let allConnections = (,) <$> rooms <*> rooms
      let sensibleConnections = filter (\(a,b) -> a/=b ) allConnections
      connections <- selectRandom numberOfConnections sensibleConnections
      return $ L.nub connections
    createCorridorsForConnections :: [(Room, Room)] -> [Room]
    createCorridorsForConnections connections = foldl (\result connection -> (createCorridor connection):result) [] connections
    updateCrossedCorridors :: [Room] -> Room -> Room
    updateCrossedCorridors corridors corridor@(Corridor legs _ crossedCorridors) =
      corridor {corridorCrossesCorridors = S.union crossedCorridors (S.fromList $ filter (roomIntersects corridor) corridors)}
    updateCrossedRooms :: [Room] -> Room -> Room
    updateCrossedRooms rooms corridor@(Corridor legs crossedRooms _) =
      corridor {corridorGoesThroughRooms = S.union crossedRooms (S.fromList $ filter (roomIntersects corridor) rooms)}

selectRandom :: Int -> [a] -> IO [a]
selectRandom howMany items = do
    stdGen <- getStdGen
    let indices = randomRs (0, (length items) - 1) stdGen
    return $ map (items !!) (take howMany indices)

createCorridor :: (Room, Room) -> Room
createCorridor (Room roomACoords@((x1a, y1a), (x2a, y2a)) _ _, Room roomBCoords@((x1b, y1b), (x2b, y2b)) _ _) = let
    (startX, startY) = getCenter roomACoords
    (endX, endY) = getCenter roomBCoords
    halfWidth = corridorWidth `div` 2
    horizontalLeg x1 x2 y = ((x1, y - 1), (x2, y + 1))
    verticalLeg x y1 y2 = if y1 < y2 then ((x - 1, y1 - 1), (x + 1, y2 + 1))
                                     else ((x - 1, y1 + 1), (x + 1, y2 - 1))
    in Corridor [horizontalLeg startX endX startY, verticalLeg endX startY endY] S.empty S.empty

getCenter :: RectBoundaries -> Coordinates
getCenter ((startX, startY), (endX, endY)) =
  ((startX + endX) `div` 2, (startY + endY) `div` 2)

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
        intersectionRangeA = grow level 1 $ roomCoordinates roomA
        intersectionRangeB = grow level 1 $ roomCoordinates roomB

grow :: Level -> Int -> RectBoundaries -> RectBoundaries
grow level howMuch (start, end) = let
    dSize = (howMuch, howMuch)
    clampToLevel = flip clampCoordinatesToLevel $ level
    in (clampToLevel $ start |-| dSize, clampToLevel $ end |+| dSize)
