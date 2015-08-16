import qualified Data.Map as Map

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [ ("betty", "554-2938")
            , ("bonnie", "452-2928")
            , ("patsy", "493-2928")
            , ("lucille", "205-2928")
            , ("wendy", "939-8282")
            , ("wendy", "939-8282")
            , ("wendy", "939-8282")
            , ("wendy", "939-8282")
            , ("wendy", "939-8282")
            , ("wendy", "939-8282")
            , ("penny", "853-2492")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- type synonyms can be parameterized.
type AssocList k v = [(k, v)]

type IntMap v = Map.Map Int v
type IntMap' = Map.Map Int

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                             then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ABCD"))
    ,(101,(Free, "EFGH"))
    ,(102,(Taken, "113"))
    ]
main :: IO ()
main = do
  print $ phoneBook
  print $ inPhoneBook "wendy" "939-8282" phoneBook
  -- we need concrete types to use stuff.
  print $ (Left 20 :: Either Int Int)
  print $ (Right 20 :: Either Int Int)
  print $ lockerLookup 101 lockers
  print $ lockerLookup 102 lockers
  print $ lockerLookup 12 lockers
