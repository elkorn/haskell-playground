main = do
    -- lists are monoids.
    print $
        [1, 2, 3] `mappend`
        [4, 5, 6]
    print $
        mconcat [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    print (mempty :: [Int])
    print $
        Product' 12 `mappend'`
        Product' 13
    print $
        Sum' 12 `mappend'`
        Sum' 13
    print $
        Any True `mappend`
        Any False `mappend`
        Any True
    print $
        All True `mappend`
        All False `mappend`
        All True
    print $ any' [True, True, False]
    print $ all' [True, True, False]
    print $ lengthCompare "hash" "map"
    print $ lengthCompare "has" "map"
    print $ lengthCompare "map" "map"
    print $ lengthCompare' "hash" "map"
    print $ lengthCompare' "has" "map"
    print $ lengthCompare' "map" "map"

class Monoid' m  where
    mempty' :: m
    mappend' :: m -> m -> m
    mconcat' :: [m] -> m
    mconcat' = foldr mappend' mempty'

newtype Product' a = Product'
    { getProduct' :: a
    } deriving (Eq,Ord,Read,Show,Bounded)

instance Num a => Monoid' (Product' a) where
    mempty' = Product' 1
    Product' x `mappend'` Product' y = Product' (x * y)

newtype Sum' a = Sum'
    { getSum' :: a
    } deriving (Eq,Ord,Read,Show,Bounded)

instance Num a => Monoid' (Sum' a) where
    mempty' = Sum' 1
    Sum' x `mappend'` Sum' y = Sum' (x + y)

newtype Any = Any
    { getAny :: Bool
    } deriving (Eq,Ord,Read,Show,Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

any' :: [Bool] -> Bool
any' = getAny . mconcat . map Any
      
newtype All = All
    { getAll :: Bool
     } deriving (Eq,Ord,Read,Show,Bounded)

instance Monoid All where
    mempty = All False
    All x `mappend` All y = All (x && y)

all' :: [Bool] -> Bool
all' = getAll . mconcat . map All

instance Monoid' Ordering where
  mempty' = EQ
  LT `mappend'` _ = LT
  EQ `mappend'` y = y
  GT `mappend'` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                        in if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend`
                     (x `compare` y)
