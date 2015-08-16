import Data.List

main = do
  print $ zipWithIndex ['a'..'z']

-- I wondered some time ago how one would implement this
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex [] = []
zipWithIndex input = zipWith (\index el -> (index, el)) [0..] input
