module Continuation where

import Control.Monad.Cont

-- For the List monad, this would give back `[11]`.
-- For the Maybe monad, this would give back `Just 11`.
-- For the `Cont` monad, this gives back something
-- that takes a function and applies it to 11.
ex1 = do
  a <- return 1
  b <- return 5
  return $ a+b

ex2 = do
  a <- return 1
  b <- (\fred -> fred 5)
  return $ a + b

test1 = runCont ex1 show

-- test2 = runCont ex2 (*2) show

i :: (Monad m) => m a -> Cont (m b) a
i x = cont (\f -> x >>= f)

run m = runCont m return

test3 = run $ do
  i $ print "Name, please."
  name <- i getLine
  i $ print $ "Hi, " ++ name

test4 = run $ do
  a <- i [1,2]
  b <- i [10,20]
  return $ a+b

main :: IO ()
main = do  
  print test1
  -- print test2
  test3
  print test4
