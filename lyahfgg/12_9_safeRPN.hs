import Data.List
import Control.Monad

main = do
    print $
        solveRPN "2 3 +"
    print $
        -- Final stack has more than one element
        solveRPN "1 2 * 8"
    print $
        -- `readMaybe` fails
        solveRPN "2 3 + GO TO HELL"

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <-
        -- Maybe [Double] here so it can be short-circuited by errors.
        foldM foldingFunction [] (words st)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return
        ((x * y) :
         ys)
foldingFunction (x:y:ys) "+" = return
        ((x + y) :
         ys)
foldingFunction (x:y:ys) "-" = return
        ((x - y) :
         ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

readMaybe :: (Read a)
          => String -> Maybe a
readMaybe st = case reads st of
        [(x,"")] -> Just x
        _ -> Nothing
