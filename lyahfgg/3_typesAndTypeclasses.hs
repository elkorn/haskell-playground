removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer is a BigInt equivalent.
factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

-- a is a type variable
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

main = do
  print $ factorial 100
  print $ circumference 4.0
  print $ isEmpty []
  print $ isEmpty [1,2,3]
-- Main> :t (==)
-- The `==` operator us a function, just as other operators.
-- If a function name is comprised only of special characters, it is treated as infix.
-- To pass an infix function around, wrap it in parentheses.

-- Main> :t (==)
-- (==) :: (Eq a) => a -> a -> Bool
-- Everything before the `=>` symbol is called a class constraint.
-- The equality function takes two values and returns a Bool - given that the type of both the values belongs to the Eq class.

-- Main> :t elem
-- elem :: (Eq a, Foldable t) => a -> t a -> Bool
-- `elem` has such constraints due to the fact that it uses == over a foldable structure (like a list) to determine whether a value resides within it.

-- `Eq` is used for types that support equality testing. Its members implement the `==` and `/=` methods.
-- When you see an `Eq` class constraint on a function, you can be sure that one of those methods is used somewhere inside it.
  print $ "Eq"
  print $ compare 5 3
-- `Ord` is a class for types that have an ordering i.e. `<`,`<=`,`>=`,`>`.
-- To be a member of the `Ord` class, a type has to also be the member of the `Eq` class.
-- `Show` allows types to be presented as strings.
  print $ "Show"
  print $ show 3
  print $ show True
  print $ show [1,2,3]
  print $ "Read"
  print $ read "3.0" * 2.0
  -- print $ read "3.0" * 2 -- will throw a "no parse" exception, the inferred type would be Int, but the string does not parse to it.
  print $ read "True" || False
  print $ (read "[1,2,3]" :: [Int])
  print $ (read "5" :: Int)
-- `Enum` members are types that are sequentially ordered, they can be used in list ranges. Members of these types have methods `succ` and `pred` implemented.
  print $ "Enum"
  print $ [LT .. GT]
-- `Bounded` class members are types that have an upper and lower bound defined.
  print $ "Bounded"
  print $ (minBound :: Int, maxBound :: Int)
  print $ (maxBound :: (Bool, Char, Int))
-- `Num` typeclass members are able to act like numbers.
-- Operators on Int, Integer, Float etc. have a (Num a) class constraint so the correct result type can be inferred from an expression using multiple `Num` members.
-- To be a member of the `Num` typeclass, a type must also belong to `Eq` and `Show`.
-- `Num` includes real and integral numbers, `Integral` includes Ints and Integers, `Floating` includes Floats and Doubles.
-- There is a convenience function: fromIntegral :: (Integral a, Num b) => a -> b. It's useful e.g. for making Integral results usable in more general expressions. One case for it's usage might be fromIntegral (length [1,2,3,4]) + 3.2 because of length :: [a] -> Int.
  print $ "Bounded"
  print $ (20 * (3 :: Integer)) -- 5 can act both as an Integer and an Int.
