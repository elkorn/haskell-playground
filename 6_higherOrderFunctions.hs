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

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'] )

main = do
  print $ compareWithHundred 10
  print $ compareWithHundred2 10
  print $ compareWithHundred3 10
  print $ compareWithHundred4 10
  print $ divideByTen 10
  print $ isUpperAlphanum 'a'
  print $ isUpperAlphanum '1'
  print $ isUpperAlphanum 'A'
