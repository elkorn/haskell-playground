import qualified Data.Map as Map
import qualified Data.Char as Char

phoneBook :: [(String, String)]
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

listLookup :: (Eq k)
           => k -> [(k, v)] -> Maybe v
listLookup _ [] = Nothing
listLookup key ((k,v):xs) = if key == k
        then Just v
        else listLookup key xs

listLookup' :: (Eq k)
            => k -> [(k, v)] -> Maybe v
listLookup' key = foldr
        (\(k,v) acc ->
              if key == k
                  then Just v
                  else acc)
        Nothing

fromList' :: (Ord k)
          => [(k, v)] -> Map.Map k v
fromList' = foldr
        (\(k,v) acc ->
              Map.insert k v acc)
        Map.empty
phoneBookToMap :: (Ord k)
               => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith
        (\number1 number2 ->
              number1 ++ ", " ++ number2)

main :: IO ()
main = do
    print $ phoneBook
    print $
        listLookup "betty" phoneBook
    print $
        listLookup' "betty" phoneBook
    print $ Map.fromList phoneBook
    -- print $ Map.empty
    print $
        Map.insert 3 100 Map.empty
    print $
        Map.insert
            5
            300
            (Map.insert
                 4
                 200
                 (Map.insert 3 100 Map.empty))
    print $
        Map.insert 5 300 $
        Map.insert 4 200 $
        Map.insert 3 100 Map.empty
    print $
        Map.insert 5 300 .
        Map.insert 4 200 .
        Map.insert 3 100 $
        Map.empty
    print $ fromList' phoneBook
    print $ Map.null Map.empty
    print $ Map.null $ fromList' phoneBook
    print $ Map.size Map.empty
    print $ Map.size $ fromList' phoneBook
    print $
        Map.singleton 3 9
    print $
        Map.insert 3 9 $
        Map.singleton 4 2
    print $
        Map.member 3 $
        Map.fromList
            [(3, 6), (4, 3), (2, 8)]
    print $
        Map.member 3 $
        Map.fromList
            [(7, 6), (4, 3), (2, 8)]
    print $
        Map.member 3 $
        Map.fromList
            [(7, 6), (4, 9), (2, 8)] -- applies a fn to all values
    print $
        Map.map (* 100) $
        Map.fromList
            [(1, 2), (2, 3)]
    print $
        Map.filter Char.isUpper $
        Map.fromList
            [(1, 'a'), (2, 'A'), (3, 'B')]
    print $ phoneBookToMap phoneBook
    print $
        Map.lookup "wendy" $
        phoneBookToMap phoneBook
    print $
        Map.fromListWith
            max
            [(2, 3), (2, 90), (1, 1), (1, 15), (3, 2), (3, 5)]
    print $
        Map.fromListWith
            (+)
            [(2, 3), (2, 90), (1, 1), (1, 15), (3, 2), (3, 5)]
    print $
        Map.insertWith (+) 3 100 $
        Map.fromList
            [(2, 3), (2, 90), (1, 1), (1, 15), (3, 2), (3, 5)]
