import Control.Monad.Instances

main = do
  print $ addStuff 3
  print $ (+) <$> (*2) <*> (+10) $ 3


-- intance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \w -> f (h w) w

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a + b)
