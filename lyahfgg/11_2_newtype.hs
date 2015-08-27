main = do
    print
        CharList
        { getCharList = "Hey hello!"
        }
    print $
        CharList "test" ==
        CharList "test"
    print $
        CharList "test" ==
        CharList "a"
    print $
        getRevTuple $
        fmap (* 100) $
        RevTuple (2, 3)
    print $
        getRevTuple $
        fmap reverse $
        RevTuple ("Release me", 123)
    -- doesn't throw because `undefined` does not get evaluated.
    print $
        head [2, 3, undefined]
    -- throws an exception
    -- print $ helloMe1 undefined
    -- does not throw an exception
    -- This is because `newtype`s can have only one constructor and Haskell does not have to evaluate the value to see if it matches (CoolBool2 _), because there can only be one constructor and one field.
    -- In case of CoolBool1 though, Haskell needs to evaluate the value to see which of the possibly many constructors will it successfully pattern match against. 
    -- There are two conclusions to be drawn here:
    -- 1. `newtype` is lazier than `data`.
    -- 2. `data` is for defining new types from scratch and `newtype` is for defining new representations of existing types.
    -- If you catch yourself using `data` for types that have one constructor and one field, consider using `newtype`.
    print $ helloMe2 undefined

-- data ZipList a = ZipList
--     { getZipList :: [a]
--     }

-- `newtype` allows a type to internally be identical as the type it is wrapping so that
-- Haskell doesn't have to do the wrapping/unwrapping overhead.
-- The limitation is that it can only have one type constructor which can only have one field.
newtype ZipList a = ZipList
    { getZipList :: [a]
    }

-- These cannot be represented with `newtype`s.
data Race
    = Human
    | Elf
    | Orc
    | Goblin
data Profession
    = Fighter
    | Archer
    | Accountant
data PlayerCharacter =
    PlayerCharacter Race
                    Profession

-- `newtype`s can be made members of typeclasses.
newtype CharList = CharList
    { getCharList :: [Char]
    } deriving (Eq,Show)

newtype RevTuple b a = RevTuple
    { getRevTuple :: (a, b)
    }

-- not possible to achieve with a normal tuple, we need a `Functor a` and the only way to get it with a tuple is to partially apply the first value, which blocks it from being used in `fmap`.
instance Functor (RevTuple c) where
    -- fmap :: (a -> b) -> RevTuple c a -> RevTuple c b
    fmap f (RevTuple (x,y)) = RevTuple (f x, y)

data CoolBool1 = CoolBool1
    { getCoolBool1 :: Bool
    }

helloMe1 :: CoolBool1 -> String
helloMe1 (CoolBool1 _) = "hello"

newtype CoolBool2 = CoolBool2
    { getCoolBool2 :: Bool
    }

helloMe2 :: CoolBool2 -> String
helloMe2 (CoolBool2 _) = "hello"
