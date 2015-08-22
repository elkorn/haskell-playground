import Data.List

-- main = interact $ show . solveRPN
main = do
  print $ ""
  -- contents <- getContents
  -- print contents
  -- print $ solveRPN contents 


solveRPN :: String -> Float 
solveRPN = head . foldl foldingFunction [] . words
                 where
                   foldingFunction :: [Float] -> String -> [Float]
                   foldingFunction (x:y:ys) "*" = (x*y):ys
                   foldingFunction (x:y:ys) "+" = (x+y):ys
                   foldingFunction (x:y:ys) "-" = (y-x):ys
                   foldingFunction (x:y:ys) "/" = (y/x):ys
                   foldingFunction (x:y:ys) "^" = (y**x):ys
                   foldingFunction (x:xs) "ln"  = log x:xs
                   foldingFunction (xs) "ln"    = [sum xs]
                   foldingFunction xs numberStr = case reads numberStr of
                                                    [] -> xs
                                                    [(x,_)] -> x:xs

