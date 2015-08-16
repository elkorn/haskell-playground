import Data.Char

main = do
  -- The key thing here is that `getContents` is lazy in reading input.
  -- It reads until EOF.
  -- These two characteristics make it perfect for reading streaming data such as stdin.
  contents <- getContents
  putStr (map toUpper contents)

-- This program can be used by `cat something | ./9_1_1_capslocker` (see `9_1_1_capslocker.sh`).
-- Another way would be to run `./9_1_1_capslocker` and write stuff.
