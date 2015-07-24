import Data.List

-- aside: foldl and foldl1 use thunks for intermediate results. This is what
-- makes them lazy, but at a cost. The thunks are put onto the stack which may
-- result in a stack overflow error for big lists. To avoid that problem, strict
-- variants of those functions are in Data.List: foldl' and foldl1'.

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

main :: IO()
main = do
  print $ numUniques [1,1,1,2]
  print $ intersperse '.' "HELLO"
  print $ intersperse 0 [1..10]
  print $ intercalate "-+-" ["XXX", "YYY", "ZZZ"]
  print $ intercalate [1,1,1] [[1..4], [5..8], [9..12]]
  print $ transpose [[1,2,3], [4,5,6], [7,8,9]]
  print $ transpose ["heidi", "heyli", "ho"]
  print $ transpose $ transpose ["heidi", "heyli", "ho"]
  -- adding polynomials:
  -- 3x^2 + 5x + 9, 10x^3 + 9 and 8x^3 + 5x^2 + x - 1
  print $ map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
  print $ concat [[3..5], [4..10], [11..16]]
  print $ concat $ concat [[[3..5], [1..8]], [[4..10], [11..16]]]
  -- the mapping function for concatMap must return a list so there is nesting.
  print $ concatMap (replicate 4 . (+5)) [1..5]
  -- this is almost the and and or you've implemented so many times in JS :)
  print $ and $ map (> 4) [5,6,7,8]
  print $ and $ map (== 4) [4,4,3,4]
  print $ or $ map (== 4) [4,5,6,7,8]
  print $ or $ map (== 4) [5,6,7,8]
  -- more convenient than and/or $ map
  print $ any (==4) [2,3,4,5]
  print $ all (> 4) [6,7,8]
  print $ all (`elem` ['A'..'Z']) "Hey hi hello"
  print $ any (`elem` ['A'..'Z']) "Hey hi hello"
  -- iterate takes a starting value and applies a function to it repeatedly,
  -- returning subsequent results
  print $ take 10 $ iterate (*2) 1
  print $ take 10 $ iterate (++ "ha") ""
  print $ splitAt 3 [1,2,3,4]
  print $ let (a,b) = splitAt 3 "foobar" in b ++ a
  print $ takeWhile (< 10) [1..]
  print $ takeWhile (/= ' ') "this is a sentence"
  print $ sum $ takeWhile (<10000) $ map (^3) [1..]
  print $ dropWhile (/= ' ') "this is a sentence"
  print $ let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
    in head $ dropWhile (\(val, y, m, d) -> val < 1000) stock
  -- span returns a pair of lists: the taken and the dropped one
  -- it spans the list while the predicate is true
  print $ let (firstWord, rest) = span (/= ' ') "This is a sentence" in "First word: " ++ firstWord ++ ", the rest: " ++ rest
  -- break bails when predicate is true
  print $ break (==4) [1..10]
  print $ span (not . (==4)) [1..10]  -- equivalent of break
  print $ sort "Hello, world!"
  -- groups adjacent elements if they are equal
  print $ group $ replicate 3 1 ++ replicate 4 2 ++ replicate 2 5
  print $ let countElements = map (\list@(x:xs)-> (x, length list)) . group . sort
      in countElements $ replicate 3 1 ++ replicate 4 2 ++ replicate 2 5
  -- `inits` and `tails` are like `init` and `tail`, but they apply recursively
  -- through the whole list
  print $ inits "hello"
  print $ tails "hello"
  print $ let str = "hello" in zip (inits str) (tails str)
  print $ search "oboe" "the last of boboets"
  print $ isInfixOf "oboe" "the last of boboets"
  print $ isPrefixOf "t" "the"
  print $ isSuffixOf "e" "the"
  -- partition splits a list based on a predicate
  print $ partition (> 3) [1,3,5,6,3,2,1,0,3,7]
  print $ partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
  print $ span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
  print $ find (> 4) [1..10]
  print $ find (> 10) [1..10]
  print $ 4 `elemIndex` [1..6]
  print $ ' ' `elemIndices` "Gimme the spaces"
  print $ findIndex (==4) [1..10]
  print $ findIndices (`elem` ['A'..'Z']) "Gimme the Caps"
  print $ zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
  print $ zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
