module Golf where

-- The functions must be as short as possible.
import Data.List
import Data.Maybe

skips :: (Eq a)
      => [a] -> [[a]]
skips xs =
  map (\x ->
         everynth ((fromJust $
                   elemIndex x xs) + 1)
                  xs)
      xs

everynth :: Int -> [a] -> [a]
everynth n xs =
  case drop (n - 1) xs of
    (y:ys) ->
      y :
      (everynth n ys)
    [] -> []
