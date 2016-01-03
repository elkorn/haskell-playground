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
    { worldHero :: Hero
    , worldDepth :: Int
    , worldLevel :: Int
    , worldLevels :: [Int]
    }

data Hero = Hero
    { heroPosition :: Coordinates
    , heroGold :: Int
    , heroHP :: Int
    , heroItems :: [Item]
    , heroOldPosition :: Coordinates
    , heroWields :: Weapon
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

startingHero :: Hero
startingHero = Hero
        (1, 1)
        0
        10
        []
        (1, 1)
        (Weapon 0 "Fists" 0)
        (Armor 0 "Rags")

startingState :: WorldState
startingState = World startingHero 0 0 []
