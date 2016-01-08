module LevelGen.Cave (generateLevel) where

import Control.Applicative
import qualified Data.Map as M
import System.Random

import Level
import Types
import Utils

-- Nested lists just for fun, althugh it's not entirely correct.
type Cell = Bool
type Cells = [[Cell]]

cellToTile :: Cell -> Tile
cellToTile True = Floor
cellToTile False = Wall

defaultChanceToStayAlive :: Int
defaultChanceToStayAlive = 45

deathThreshold = 3
birthThreshold = 4
simulationSteps = 1

generateLevel :: LevelSpec -> IO Level
generateLevel spec = do
  cells <- cellMap 10 10
  let cave = simulateCave cells simulationSteps
  return $ emptyLevel {levelMax = (80, 40), levelTiles = M.fromList $ cellsToTiles cave}

-- This is clunky. What would work better here?
map2d :: (Coordinates -> a -> b) -> [[a]] -> [[b]]
map2d f = map (\(y, row) -> map (\(x, cell) -> f (x,y) cell) $ zipWithIndex row) . zipWithIndex

-- printCells :: Cells -> IO ()
-- printCells cellMap = do
--   printTiles $ M.fromList $ concat $ map2d (\coordinates cell -> (coordinates, cellToTile cell)) cellMap

cellMap :: Int -> Int -> IO Cells
cellMap x y = do
  stdGen <- getStdGen
  let coordinates = (,) <$> [0 .. x-1] <*> [0 .. y-1]
  let cells = take (x*y) $ map (< defaultChanceToStayAlive) $ (randomRs (0, 99) stdGen :: [Int])
  return [[cells !! ((x')*y+y') | x' <- [0..x-1]] | y' <- [0..y-1]]


neighbourhood :: [Coordinates]
neighbourhood = filter (/= (0,0)) [(x,y) | x <- [-1..1], y <- [-1..1]]

countAliveNeighbours :: Cells -> Coordinates -> Int
countAliveNeighbours cells coordinates = foldl (+) 0 $ map getNeighbourAlive neighbourhood
  where getNeighbourAlive :: Coordinates -> Int
        getNeighbourAlive (0,0) = 0
        getNeighbourAlive dCoordinates =
          if isInList then 1 else 0
          where isInList = x >= 0 && y >= 0 && x < (length cells) && y < (length $ cells !! x)
                (x,y) = coordinates |+| dCoordinates

simulationStep :: Cells -> Cells
simulationStep cells = map2d applyEvolution  cells
  where applyEvolution :: Coordinates -> Cell -> Cell
        applyEvolution (x,y) cell = createNewCell cell $ countAliveNeighbours cells (x,y)
        createNewCell :: Cell -> Int -> Cell
        createNewCell True aliveNeighbours
          | aliveNeighbours < deathThreshold = False
          | otherwise = True
        createNewCell False aliveNeighbours
          | aliveNeighbours >= birthThreshold = True
          | otherwise = False

simulateCave :: Cells -> Int -> Cells
simulateCave cave 0 = cave
simulateCave cave n = simulateCave (simulationStep cave) (n-1)

cellsToTiles :: Cells -> [(Coordinates, Tile)]
cellsToTiles = concat . map2d cellToTile
  where cellToTile :: Coordinates -> Cell -> (Coordinates, Tile)
        cellToTile coordinates True = (coordinates, Floor)
        cellToTile coordinates False = (coordinates, Wall)
