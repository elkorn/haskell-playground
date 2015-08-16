-- The elements that are defined non-recursively within a recursion are called an edge condition.

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' list = case list of
     []     -> error "Maximum of empty list"
     [x]    -> x
     (x:xs) -> max x (maximum'' xs) 

-- `Num` is not a subclass of `Ord` == what constitutes for a number does not necessarily have to adhere to an ordering.
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = [x] ++ repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [y | y <- xs, y <= x]
        largerSorted = quicksort [y | y <- xs, y > x]
    in  smallerSorted ++ [x] ++ largerSorted

main = do
  print $ maximum' [1 .. 20]
  print $ maximum'' [1 .. 20]
  print $ replicate' 12 42
  print $ take' 3 [1 .. 20]
  print $ reverse' [1 .. 20]
  print $ take' 3 (repeat' 42)
  print $ zip' [1 .. 20][20 .. 40]
  print $ zip' [1 .. 20][20 .. 21]
  print $ elem' 12 [1 .. 20]
  print $ elem' 21 [1 .. 20]
  print $ quicksort [20, 19, 18, 1]
  print $ quicksort "lorem ipsum dolor sit amet"
