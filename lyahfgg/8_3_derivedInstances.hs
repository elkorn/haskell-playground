data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving (Eq,Show,Read)

-- The order of constructor definition matters in Ord.
data B
    = F
    | T
    deriving (Eq,Ord)

-- `Enum` members have predecessors and successors.
-- `Bounded` members have a lowest and highest possible value.
data Day
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq,Ord,Show,Read,Bounded,Enum)

main :: IO ()
main = do
    mikeD <-
        return $
        Person
        { firstName = "Michael"
        , lastName = "Diamond"
        , age = 43
        }
    adRock <-
        return $
        Person
        { firstName = "Adam"
        , lastName = "Horovitz"
        , age = 41
        }
    print $ mikeD == mikeD
    print $ adRock == mikeD
    print $
        mikeD `elem`
        [adRock, mikeD]
    print $ adRock
    mikeD' <-
        return $
        read "Person {firstName = \"Michael\", lastName=\"Diamond\", age=43}"
    print $ mikeD == mikeD'
    print $ T `compare` F
    -- `Nothing` is defined before `Just` so it will always be LT.
    print $
        Nothing <
        Just 12
    print $
        Nothing <
        Just (-12)
    print $
        (read "Saturday" :: Day)
    print $ Monday `compare` Wednesday
    print $
        (minBound :: Day)
    print $
        (maxBound :: Day)
    print $
        succ Monday
    print $
        pred Sunday
    print $
        [Monday .. Thursday]
    print $
        ([minBound .. maxBound] :: [Day])
