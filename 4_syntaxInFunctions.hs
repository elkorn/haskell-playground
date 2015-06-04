sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- Main> charName 'd'
-- "*** Exception: 4_syntaxInFunctions.hs:(14,1)-(16,22): Non-exhaustive patterns in function charName

-- Pattern matching on tuples. Note that binding several variables requires for them to be put in parentheses even if using `_`.
first :: (a,b,c) -> a
first (a,_,_) = a
second :: (a,b,c) -> b
second  (_,b,_) = b
third :: (a,b,c) -> c
third  (_,_,c) = c

-- Pattern matching on lists.
head' :: [a] -> a
head' [] = error "Head of empty list"
head' (h:_) = h

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:tail) = 1 + length' tail

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (head:tail) = head + sum' tail

headWithAsPattern :: String -> String
headWithAsPattern [] = error "Head of empty list"
headWithAsPattern all@(x:xs) = "The head of " ++ all ++ " is " ++ [x]

-- Guards seem to effectively boil down to a switch(true) equivalent.
bmiTell :: (RealFloat a) => a -> String 
bmiTell bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal weight"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Kraken"

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | weight / height ^ 2 <= 18.5 = "Underweight"
    | weight / height ^ 2 <= 25.0 = "Normal weight"
    | weight / height ^ 2 <= 30.0 = "Overweight"
    | otherwise                   = "Kraken"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT


-- Note that the variables in the where block have to be aligned for Haskell to understand that they all belong there.
bmiTell3 :: (RealFloat a) => a -> a -> String
bmiTell3 weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal weight"
    | bmi <= fat    = "Overweight"
    | otherwise     = "Kraken"
    where bmi = weight / height ^ 2 -- These are called where bindings.
          (skinny, normal, fat) = (18.5, 25.0, 30.0) -- Pattern matching also works here.

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
-- Functions can also defined within the where block.
-- where blocks can be nested. It's a common practice to put helper functions in nested where blocks.
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where
       bmi weight height = weight `divide` (height ^ 2)
       divide a b = a / b

calcBmis2 :: (RealFloat a) => [(a,a)] -> [a]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- This is to show that the variables from a `let` binding can be used in the matcher as well.
calcBmisOfFatPpl :: (RealFloat a) => [(a,a)] -> [a]
calcBmisOfFatPpl xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


-- The difference between `let` and `where` bindings are that `let` bindings are expressions themselves and `where` bindings are just a syntactic construct that let you bind variables at the end so that the whole function can see them. `let` bindings are local - they do not span across guards.
-- When writing `let` bindings in GHCi the `in` par can be omitted - the variable will be bound to the interactive sessions scope.
cylinderArea :: (RealFloat a) => a -> a -> a
cylinderArea r h =
    let sideArea = 2 * pi * r * h
        topArea  = 2 * pi * r^2
    in  sideArea + 2 * topArea

-- Using case expressions for pattern matching is interchangeable with pattern matching throuh multiple function definitions.
head'' :: [a] -> a
head'' xs = case xs of
     []    -> error "Head of empty list"
     (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
     [] -> "empty."
     [x] -> "a singleton list."
     xs -> "a longer list."
     
describeList2 :: [a] -> String
describeList2 xs = "The list is " ++ what xs
    where
        what []  = "empty."
        what [x] = "a singleton list."
        what xs  = "a longer list."

main = do
  print $ addVectors (1,2) (3,4)
-- Pattern matching can also be used in list comprehensions. When a match fails, it moves on to the next element.
  print $ "Pattern matching"
  print $ [a+b | (a,b) <- [(1,2), (3,4)]]
  print $ head' [3,4,5]
  print $ head' "Hello"
  print $ length' [3,4,5]
  print $ tell [3,4,5]
  print $ sum' [3,4,5]
  print $ sum' [3.0,4.0,5.0] -- Making this return an Int is not as simple as defining sum :: (Num a, Num b) => [a] -> b
  print $ headWithAsPattern "Robert"
  print $ "Guards"
  print $ bmiTell 25
  print $ bmiTell 40
  print $ bmiTell2 80 1.89
  print $ max 'a' 'A'
  print $ "`where` and `let` bindings"
  print $ bmiTell3 80 1.89
  print $ initials "Bill" "Gates"
  print $ calcBmis [(80, 1.89), (80, 1.5), (60, 1.95), (120, 1.6)]
  print $ cylinderArea 12 13
  print $ (4 * (if 5 > 3 then 5 else 3))
  print $ (4 * (let a = 9 in a + 1))
  print $ let square x = x * x in (square 2, square 3, square 4)
  print $ (let a = 100; b = 200; c = 300; in a*b*c, let foo="GL"; bar="HF" in foo ++ bar)
  print $ calcBmis2 [(80, 1.89), (80, 1.5), (60, 1.95), (120, 1.6)]
  print $ calcBmisOfFatPpl [(80, 1.89), (80, 1.5), (60, 1.95), (120, 1.6)]
  print $ "Case expressions"
  print $ head'' [3,4,5]
  print $ head'' "Hello"
  print $ describeList []
  print $ describeList [1]
  print $ describeList [1,2,3]
  print $ describeList2 []
  print $ describeList2 [1]
  print $ describeList2 [1,2,3]


