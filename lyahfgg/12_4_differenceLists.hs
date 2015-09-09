import Control.Monad.Writer

main = do
    print $
        fromDiffList
            (toDiffList
                 [1, 2, 3] `mappend`
             toDiffList
                 [4, 5, 6])
    mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34

-- Difference lists represent lists as append functions.
-- Having them as such allows constructing lists from sublists efficiently.
newtype DiffList a = DiffList
    { getDiffList :: [a] -> [a]
    }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList
            (\xs ->
                  [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList $ f . g

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
      tell (toDiffList ["Finished with" ++ show a])
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result
