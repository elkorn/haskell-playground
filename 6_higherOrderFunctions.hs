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

