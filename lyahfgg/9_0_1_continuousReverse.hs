main = do
  line <- getLine
  if null line      -- every `if` must have an `else` - every expr must have a value.
     then do return () -- `return` makes an `IO` action out of a pure value.
          else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- note that the space operator has higher precedence than composition.
-- this is equivalent to
-- reverseWords st = unwords (map reverse (words st))
