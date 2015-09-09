import Data.Monoid
import Control.Monad.Writer

main = do
    print $ "hello"
    print $
        isBigGang 10
    print $
        (10, "test") `applyLog`
        (\x ->
              (x * 2, "test 2"))
    print $
        ("beans", Sum 10) `applyLog`
        addDrink
    print $
        ("beans", Sum 10) `applyLog`
        addDrink
        `applyLog`
        addDrink
    print $ runWriter (return 3 :: Writer String Int)
    print $ runWriter (return 3 :: Writer (Sum Int) Int)
    print $ runWriter (return 3 :: Writer (Product Int) Int)
    print $ runWriter (return 3 :: Writer [()] Int)
    print $ multWithLog
    print $ fst $ runWriter (gcd' 8 3)
    mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)

isBigGang :: Int -> Bool
isBigGang = (> 9)

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' = applyLog

applyLog :: (Monoid m)
         => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x,log') f = let (y,newLog) = f x
    in (y, log' `mappend` newLog) 

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 125)
addDrink _= ("beer", Sum 55)


-- newtype Writer' w a = Writer' { runWriter' :: (a, w) }
-- TODO how to Applicative?
-- instance (Monoid w) => Monad (Writer' w) where
--   return x = Writer' (x, mempty)
--   (Writer' (x, v)) >>= f = let
--     (Writer' (y, v')) = f x
--     in Writer' (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Heyyy"]
  return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

-- Inefficient,  usage of `++` is associated to the left and appending to long lists is inefficient.
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
      tell ["Finished with" ++ show a]
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell [show a ++ "mod " ++ show b ++ " = " ++ show (a `mod` b)]
      return result
