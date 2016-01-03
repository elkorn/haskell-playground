module Level where

import qualified Data.Map as M
import Types

populate :: Level -> (Coordinates, Char) -> Level
populate level (coordinates, tile) =
    case tile of
        '#'   -> level { levelTiles = M.insert coordinates Wall            t }
        '>'   -> level { levelTiles = M.insert coordinates (St Downstairs) t }
        '<'   -> level { levelTiles = M.insert coordinates (St Upstairs)   t }
        '+'   -> level { levelTiles = M.insert coordinates (Dr Closed)     t }
        '-'   -> level { levelTiles = M.insert coordinates (Dr Open)       t }
        '~'   -> level { levelTiles = M.insert coordinates Acid            t }
        ' '   -> level { levelTiles = M.insert coordinates Floor           t }
        _     -> level
        where t = levelTiles level

parseMap :: [String] -> Level
parseMap str = let
  -- Infinite stream of coordinates
  coordinates = [[(x,y) | x <- [0..]] | y <- [0..]]
  -- each character of the input string paired with related coordinates
  asciiMap = concat $ zipWith zip coordinates str
  maxX = maximum . map (fst . fst) $ asciiMap
  maxY = maximum . map (snd . fst) $ asciiMap
  maxCoordinates = (maxX, maxY)
  in foldl populate (emptyLevel {levelMax = maxCoordinates}) asciiMap

updateLevelMax :: Level -> Level
updateLevelMax level@(Level _ _ _ _ _ tiles _) = let
  maxX = maximum . map (fst . fst) $ M.toList tiles 
  maxY = maximum . map (snd . fst) $ M.toList tiles
  maxCoordinates = (maxX, maxY)
  in level {levelMax = maxCoordinates }
  

map1 :: [String]
map1   = [ "##############"
         , "#>...........#          ######"
         , "#............############....#"
         , "#............-..........+....#"
         , "#....~~......############....#"
         , "#.....~~.....#          #....#"
         , "#......~~....#          #.<..#"
         , "##############          ######" ]

level1 :: Level
level1 = parseMap map1

type LevelPositionLookup =
  Coordinates ->
  Level ->
  Bool

levelLookup :: (Eq a) =>
  (Level -> M.Map Coordinates a) ->
  a -> LevelPositionLookup
levelLookup accessor searchFor coordinates level =
  case M.lookup coordinates $ accessor level of
    Just found -> searchFor == found
    _ -> False

existLevelLookup :: 
  (Level -> M.Map Coordinates a) ->
  LevelPositionLookup
existLevelLookup accessor coordinates level =
  M.member coordinates $ accessor level

isTileType :: Tile -> LevelPositionLookup
isTileType = levelLookup levelTiles

-- PROBLEM: nested data types, would have to use wildcards in expression context
--          to define it the same as tile predicates.
-- isItemType :: Item -> Coordinates -> Level -> Bool
-- isItemType = levelLookup levelItems
-- NOTE: Obviously not working, but there may be something in that direction.
-- itemLookup ::
--   (Level -> M.Map Coordinates a) ->
--   (a -> Item) ->
--   Coordinates ->
--   Level ->
--   Bool
-- itemLookup accessor searchFor coordinates level =
--   case M.lookup coordinates $ accessor level of
--     Just (searchFor _) -> True
--     _ -> False

isAcid = isTileType Acid
isWall = isTileType Wall
isOpenDoor = isTileType (Dr Open)
isClosedDoor = isTileType (Dr Closed)
isDownstairs = isTileType (St Downstairs)
isUpstairs = isTileType (St Upstairs)
isFloor = isTileType Floor

isArmor :: LevelPositionLookup
isArmor coordinates level = case M.lookup coordinates $ levelItems level of
  Just (Arm _) -> True
  _ -> False

isWeapon :: LevelPositionLookup
isWeapon coordinates level = case M.lookup coordinates $ levelItems level of
  Just (Weap _) -> True
  _ -> False

isPotion :: LevelPositionLookup
isPotion coordinates level = case M.lookup coordinates $ levelItems level of
  Just (Pot _) -> True
  _ -> False

isMonster = existLevelLookup levelMonsters
isGold = existLevelLookup levelGold
