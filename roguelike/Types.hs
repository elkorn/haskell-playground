module Types where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

import qualified Data.Map as M

type Coordinates = (Int, Int)

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Show,Eq)

data Input
    = Dir Direction
    | Wait
    | Exit
    | Restart
    deriving (Show,Eq)

data LevelSize
    = Small
    | Medium
    | Large
    | Huge
      deriving Show

-- This will be used to add gold, monsters etc.
data LevelSpec = LevelSpec LevelSize

data WorldState = World
    { worldHero :: Hero
    , worldDepth :: Int
    , worldLevel :: Level
    , worldLevels :: [Level]
    }

data Hero = Hero
    { heroPosition :: Coordinates
    , heroGold :: Int
    , heroHP :: Int
    , heroItems :: [Item]
    , heroOldPosition :: Coordinates
    , heroWields :: Weapon
    , heroWears :: Armor
    } deriving Show

data Level = Level
    { levelDepth :: Int
    , levelGold :: M.Map Coordinates Int
    , levelItems :: M.Map Coordinates Item
    , levelMapped :: M.Map Coordinates Bool
    , levelMax :: Coordinates
    , levelTiles :: M.Map Coordinates Tile
    , levelMonsters :: M.Map Coordinates Monster
    } deriving Show

data Monster = Monster
    { monsterPosition :: Coordinates
    , monsterGold :: Int
    , monsterHP :: Int
    , monsterItems :: [Item]
    , monsterOldPosition :: Coordinates
    } deriving Show

data Item
    = Arm Armor
    | Pot Potion
    | Weap Weapon
      deriving Show

data Armor = Armor
    { armorDefense :: Int
    , armorDest :: String
    } deriving Show

data Potion = Potion
    { potionAmount :: Int
    , potionDesc :: String
    , potionEffect :: Effect
    } deriving Show

data Effect
    = Harm
    | Heal
      deriving Show

data Weapon = Weapon
    { weaponDamage :: Int
    , weaponDesc :: String
    , weaponToHit :: Int
    } deriving Show

data Tile
    = Acid
    | Dr Door
    | St Stairs
    | Wall
    | Floor
      deriving (Eq, Show)

data Door
    = Closed
    | Open
      deriving (Eq, Show)


data Stairs
    = Downstairs
    | Upstairs
      deriving (Eq, Show)

startingHero :: Hero
startingHero = Hero
        (0, 0)
        0
        10
        []
        (0, 0)
        (Weapon 0 "Fists" 0)
        (Armor 0 "Rags")

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

startingState :: WorldState
startingState = World startingHero 0 emptyLevel [emptyLevel]

data GameConfig = GameConfig

type Game = ReaderT GameConfig (StateT WorldState IO)
