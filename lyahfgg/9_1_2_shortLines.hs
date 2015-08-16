main = do
  -- contents <- getContents
  -- putStr $ shortLinesOnly contents
  -- interact shortLinesOnly
  interact shortLinesOnly'

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines   = lines input
      shortLines = filter (\line -> length line < 10) allLines
      in unlines shortLines

shortLinesOnly' :: String -> String
shortLinesOnly' = unlines . filter ((>10) . length) . lines
