-- data Person =
--     Person String
--            String
--            Int
--            Float
--            String
--            String
--     deriving (Show)

-- firstName, lastName, phoneNumber, flavor :: Person -> String
-- firstName (Person f _ _ _ _ _) = f
-- lastName (Person _ l _ _ _ _) = l
-- phoneNumber (Person _ _ _ _ pn _) = pn
-- flavor (Person _ _ _ _ _ f) = f

-- age :: Person -> Int
-- age (Person _ _ a _ _ _) = a

-- height :: Person -> Float
-- height (Person _ _ _ h _ _) = h

data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
    } deriving (Show)

data Car = Car
    { company :: String
    , model :: String
    , year :: Int
    } deriving (Show)

main :: IO ()
main = do
    guy <-
        return $
        Person "BJ" "Blaskovitz" 43 184.2 "123-1234" "Beer"
    car <-
        return $
        Car
        { company = "Ford"
        , model = "Mustang"
        , year = 1967
        }
    print $ guy
    print $ firstName guy
    print $ lastName guy
    print $ age guy
    print $ height guy
    print $ phoneNumber guy
    print $ flavor guy
    print $ car
