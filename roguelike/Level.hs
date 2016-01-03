module Level where

import qualified Data.Map as M
import Types

emptyLevel :: Level
emptyLevel = Level
    { levelDepth = 0
    , levelGold = M.empty
    , levelItems = M.empty
    , levelMapped = M.empty
    , levelMax = (1,1)
    , levelTiles = M.empty
    , levelMonsters = M.empty
    }

populate :: Level -> (Coordinates, Char) -> Level
populate level (coordinates, tile) =
    case tile of
        '#'   -> level { levelTiles = M.insert coordinates Wall            t }
        '>'   -> level { levelTiles = M.insert coordinates (St Downstairs) t }
        '<'   -> level { levelTiles = M.insert coordinates (St Upstairs)   t }
        '+'   -> level { levelTiles = M.insert coordinates (Dr Closed)     t }
        '-'   -> level { levelTiles = M.insert coordinates (Dr Open)       t }
        '~'   -> level { levelTiles = M.insert coordinates Acid            t }
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

map1 :: [String]
map1   = [ "##############"
         , "#>           #          ######"
         , "#            ############    #"
         , "#            -          +    #"
         , "#    ~~      ############    #"
         , "#     ~~     #          #    #"
         , "#      ~~    #          # <  #"
         , "##############          ######" ]

level1 :: Level
level1 = parseMap map1

levelLookup ::
  (Level -> M.Map Coordinates a) ->
  a ->
  Coordinates ->
  Level ->
  Bool
levelLookup accessor searchFor coordinates level =
  case M.lookup coordinates $ accessor level of
    Just searchFor -> True
    _ -> False

existLevelLookup :: 
  (Level -> M.Map Coordinates a) ->
  Coordinates ->
  Level ->
  Bool
existLevelLookup accessor coordinates level =
  M.member coordinates $ accessor level

isTileType :: Tile -> Coordinates -> Level -> Bool
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
isDoorOpen = isTileType (Dr Open)
isDoorClosed = isTileType (Dr Closed)
isDownstairs = isTileType (St Downstairs)
isUpstairs = isTileType (St Upstairs)

isArmor :: Coordinates -> Level -> Bool
isArmor coordinates level = case M.lookup coordinates $ levelItems level of
  Just (Arm _) -> True
  _ -> False

isWeapon :: Coordinates -> Level -> Bool
isWeapon coordinates level = case M.lookup coordinates $ levelItems level of
  Just (Weap _) -> True
  _ -> False


isPotion :: Coordinates -> Level -> Bool
isPotion coordinates level = case M.lookup coordinates $ levelItems level of
  Just (Pot _) -> True
  _ -> False

isMonster = existLevelLookup levelMonsters
isGold = existLevelLookup levelGold
