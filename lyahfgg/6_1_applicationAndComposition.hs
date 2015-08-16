-- function application
-- ($) :: (a->b) -> a -> b
-- f $ x = f x
-- The difference between `$` and ` ` is that
-- ` ` has the highest possible precedence and
-- `$` has the lowest possible precedence.
-- In other words, ` ` is left-associative and `$` is right-associative.

-- function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f g x

-- the composition operator helps with tacit programming.
fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50

-- sum of odd squares smaller than 10000
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum'' :: Integer
oddSquareSum'' =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile $ (< 10000) oddSquares
  in  sum belowLimit

main :: IO()
main = do
  print $ foldl1 (+) [1,2,3,4]
  print $ sqrt 9 + 3 + 4
  print $ sqrt (9 + 3 + 4)
  print $ sqrt $ 9 + 3 + 4
  -- `$` is convenient for avoiding writing so much parens
  print $ scanl (+) 0 (filter (> 10) (map (*2) [2..10]))
  print $ scanl (+) 0 $ filter (> 10) $ map (*2) [2..10]
  -- here we map function application over a list of functions.
  print $ map ($ 3) [(+4), (*10), (^2), sqrt]
  print $ map ($ 3) [(+4), (*10), (^2), sqrt]
  print $ map (\x -> negate (abs x)) [-10..10]
  print $ map (negate . abs) [-10..10]
  print $ map (negate . sum . tail) [[1..5], [3..6], [1..7]]
  print $ sum (replicate 5 (max 6 8))
  print $ (sum . replicate 5 . max 6) 8
  print $ sum . replicate 5 . max 6 $ 8
  print $ fn 20
  print $ fn' 20
  print $ oddSquareSum
  print $ oddSquareSum'
  print $ oddSquareSum''
