import qualified Data.Char as Char
import Control.Monad

-- main :: IO ()
-- By convention, we don't usually specify a type declaration for `main`.
main = do
  putStrLn "What's your name?"
  -- The only way to get data out of an `IO` action is through the binding `<-` operator.
  -- Furthermore, this can be done only within another `IO` action.
  -- Thus, we have a clean separation between pure and impure code.
  -- :t getLine
  -- getLine :: IO String
  -- name <- getLine
  -- Writing `name = getLine` would just return a value of `IO String`.
  -- The last action cannot be bound to a name.
  -- putStrLn $ "Hello, " ++ name ++ "."
  -- `let` bindings can be used without the `in` part within a `do` block.
  -- This is because a `do` block is such an expression itself
  -- let bigName = map Char.toUpper name
  -- `<-` only has to be used to bind results of monads. Pure expressions don't need that.
  -- putStrLn $ "Hello, " ++ bigName ++ "."
  putStr "Test, "
  putStrLn "fest"
  putChar 'a'
  putStr' "Hell yeah"
  print' 12
  -- readCharsTillSpace
  -- readCharsTillSpace'
  -- rs <- sequence [getLine, getLine, getLine]
  -- print rs
  sequence (map print [1..10])
  -- equivalent
  mapM print [1..10]
  -- same, but throws away the result
  mapM_ print [1..10]
  -- forever $ do
  --   putStrLn "FOREVER"
  forM [1..10] print
  forM_ [1..10] print
  numbers <- forM [1..3] (\n -> do
                   return $ "The number is " ++ show n)
  print numbers
  mapM_ putStrLn numbers

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

print' :: (Show a) => a -> IO ()
print' = putStrLn . show

-- Character reading will only start to happen when `return` is pressed.
readCharsTillSpace :: IO ()
readCharsTillSpace = do
  c <- getChar
  if c /= ' '
     then do
       putChar c
       readCharsTillSpace
     else do return ()


-- `when` comes from `Control.Monad`.
readCharsTillSpace' :: IO ()
readCharsTillSpace' = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    readCharsTillSpace'
