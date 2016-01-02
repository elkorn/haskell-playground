module Types where
import qualified Data.Map as M

type Coordinates = (Int, Int)

data Input
    = Up
    | Down
    | Left
    | Right
    | Exit
    deriving (Show,Eq)

data WorldState = World
    { wHero :: Hero
    , wDepth :: Int
    , wLevel :: Int
    , wLevels :: [Int]
    }

data Hero = Hero
    { heroPosition :: Coordinates
    , heroGold :: Int
    , heroHP :: Int
    , heroItems :: [Item]
    , heroOldPosition :: Coordinates
    , heroWield :: Weapon
    , heroWears :: Armor
    }

data Level = Level
    { levelDepth :: Int
    , levelGold :: M.Map Coordinates Int
    , levelItems :: M.Map Coordinates Item
    , levelMapped :: M.Map Coordinates Bool
    , levelMax :: Coordinates
    , levelTiles :: M.Map Coordinates Tile
    , levelMonsters :: M.Map Coordinates Monster
    }

data Monster = Monster
    { monsterPosition :: Coordinates
    , monsterGold :: Int
    , monsterHP :: Int
    , monsterItems :: [Item]
    , monsterOldPosition :: Coordinates
    }

data Item
    = Arm Armor
    | Pot Potion
    | Weap Weapon

data Armor = Armor
    { armorDefense :: Int
    , armorDest :: String
    }

data Potion = Potion
    { potionAmount :: Int
    , potionDesc :: String
    , potionEffect :: Effect
    }

data Effect
    = Harm
    | Heal

data Weapon = Weapon
    { weaponDamage :: Int
    , weaponDesc :: String
    , weaponToHit :: Int
    }

data Tile
    = Acid
    | Dr Door
    | St Stairs
    | Wall

data Door
    = Closed
    | Open

data Stairs
    = Downstairs
    | Upstairs
