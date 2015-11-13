module Golf where

-- The functions must be as short as possible.
import Data.List
import Data.Maybe

skips :: (Eq a)
      => [a] -> [[a]]
skips xs =
  map (\x ->
         everynth ((fromJust $
                    elemIndex x xs) +
                   1)
                  xs)
      xs

everynth :: Int -> [a] -> [a]
everynth n xs =
  case drop (n - 1) xs of
    (y:ys) ->
      y :
      (everynth n ys)
    [] -> []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y > x && y > z = y : rest
  | otherwise = rest
  where rest = (localMaxima $ y : z : zs)
localMaxima _ = []

histogram :: [Int] -> String
histogram xs = hBody ++ hTail
  where numRange = [0 .. 9]
        hHeight :: Int
        hHeight = maximum xs
        hBody :: String
        hBody =
          concat $
          map hRow $
          reverse [0 .. hHeight]
        hRow :: Int -> String
        hRow lvl =
          concat $
          (map (\n ->
                  if ((length $
                       elemIndices n xs) >
                      lvl)
                     then "*"
                     else " ")
               numRange) ++
          ["\n"]
        hTail :: String
        hTail =
          concat $
          (map (\_ -> "=") numRange) ++
          ["\n"] ++
          (map show numRange) ++
          ["\n"]
