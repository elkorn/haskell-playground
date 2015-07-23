import Data.List
-- import only specific functions
-- import Data.List (nub, sort)
-- import all functions except specified
-- import Data.List hiding (nub)

-- to avoid naming clashes, imports can be qualified.
import qualified Data.Map as M

-- Core module documentation: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/

-- aside: foldl and foldl1 use thunks for intermediate results. This is what
-- makes them lazy, but at a cost. The thunks are put onto the stack which may
-- result in a stack overflow error for big lists. To avoid that problem, strict
-- variants of those functions are in Data.List: foldl' and foldl1'.

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

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
  print $ zipWith [5..1] map (!!) [5..1]
