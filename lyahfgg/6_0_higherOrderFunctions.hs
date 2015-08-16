-- Space is sort of like a function application operator and it has the highest precedence.

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred2 :: (Num a, Ord a) => a -> Ordering
compareWithHundred2 = compare 100

compareWithHundred3 :: (Num a, Ord a) => a -> Ordering
compareWithHundred3 = (100 `compare`)

compareWithHundred4 :: (Num a, Ord a) => a -> Ordering
compareWithHundred4 = (`compare` 100)

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenby = (10/)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'] )

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice2 :: (a -> a) -> a -> a
applyTwice2 f x = f . f $ x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f(x) : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort' (filter (<= x) xs)
        largerSorted  = quicksort' (filter (> x) xs)
    in  smallerSorted ++ [x] ++ largerSorted

largestDivisible :: (Integral a) => a -> a -> a
largestDivisible by from = head (filter p [from, from-1..])
    where p x = x `mod` by == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

sum' :: (Integral a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain(n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

sum'' :: (Integral a) => [a] -> a
sum'' xs = foldl (+) 0 xs

tacitSum :: (Integral a) => [a] -> a
tacitSum = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a->b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- can be implemented via foldl, but ++ is much more expensive than :
map''' :: (a->b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- foldl1 and foldr1 are variants that assume the first element of the list as the initial acc value.
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: (Ord a) => [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

reverse'' :: (Ord a) => [a] -> [a]
reverse'' = foldl (flip (:) )[]

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

head'' :: [a] -> a
head'' = foldl1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

last'' :: [a] -> a
last'' = foldr1 (\_ x -> x)

-- how many elements does it take for the sum of the roots of all natural
-- numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length(takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- map sqrt [1..]     -> get the sqrts of all natural numbers
-- scanl 1 (+)        -> see the consecutive values of the sum
-- takeWhile (< 1000) -> work while elements are < 1000
-- length             -> :)
-- +1                 -> X elements are < 1000, so we need X+1 to exceed 1000
main :: IO()
main = do
  print $ compareWithHundred 10
  print $ compareWithHundred2 10
  print $ compareWithHundred3 10
  print $ compareWithHundred4 10
  print $ divideByTen 10
  print $ isUpperAlphanum 'a'
  print $ isUpperAlphanum '1'
  print $ isUpperAlphanum 'A'
  print $ divideTenby 2
  print $ applyTwice (/2) 20
  print $ applyTwice2 (/2) 20
  print $ zipWith (+) [1..10] [10..20]
  print $ flip' (/) 10 5
  print $ map' (/2) [1..20]
  print $ filter' (even) [1..20]
  print $ let notNull x = not (null x) in filter notNull [[1..20], [], [30..40]]
  print $ quicksort' [20,1, 25, 14]
  print $ largestDivisible 3829 100000
  print $ takeWhile' (<3) [1..20]
  print $ sum' [1..5]
  print $ sum' (takeWhile' (<10000) [n^2 | n <- [1..], odd(n ^ 2)])
  print $ sum' (takeWhile' (<10000) (filter odd (map (^2) [1..])))
  print $ chain 10
  print $ chain 1
  print $ chain 3
  print $ numLongChains
  -- See how the range gets mapped to a list of functions
  print $ let listOfFuns = map (*) [0..] in (listOfFuns !! 4) 15
  -- print $ map (*) [0..] $ [1,2,3]
  print $ zipWith (\a b -> (a * 30 +3) / b) [5,2, 1] [1,2,5]
  print $ flip'' (/) 10 5
  print $ sum'' [1,2,3,4,5]
  print $ tacitSum [1,2,3,4,5]
  print $ elem' 4 [1,2,3,4]
  print $ map'' (+2) [1,2,3,4]
  print $ map''' (+2) [1,2,3,4]
  print $ maximum' [1,2,3,4]
  print $ reverse' [1,2,3,4]
  print $ product' [1,2,3,4]
  print $ head' [1,2,3,4]
  print $ head'' [1,2,3,4]
  print $ last' [1,2,3,4]
  print $ last'' [1,2,3,4]
  print $ reverse'' [1,2,3,4]
  -- scanl and scanr are like folds but they report the intermediate accumulator states
  print $ scanl (+) 0 [1,2,3,4]
  print $ scanr (+) 0 [1,2,3,4]
  print $ sqrtSums
  print $ sum (map sqrt [1..131])
  print $ sum (map sqrt [1..130])
