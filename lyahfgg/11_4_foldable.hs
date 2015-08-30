import qualified Data.Foldable as F

{-
  The difference between fold types:
  foldr :: (a -> b -> b) -> b -> [a] -> b

  F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

 The 'default' `foldr` is bound to a list type, the other one can fold over anything.
-}
main = do
    print $
        F.foldl (+) 2 $
        Just 9
    print $
        F.foldl (+) 2 $
        Nothing
    print $
        F.foldr (||) False $
        Just True
    let testTree = Node
                10
                (leaf 2)
                (leaf 13)
    print $
        F.foldl (+) 0 testTree
    print $
        getAny $
        F.foldMap
            (\x ->
                  Any $ x == 3)
            testTree
    print $
        getAny $
        F.foldMap
            (\x ->
                  Any $ x > 3)
            testTree
    print $
        F.foldMap
            (\x ->
                  [x])
            testTree

data Tree a
    = Empty
    | Node a
           (Tree a)
           (Tree a)
           deriving
           (Show, Read, Eq)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

instance F.Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

newtype Any = Any
    { getAny :: Bool
    } deriving (Eq,Ord,Read,Show,Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

any' :: [Bool] -> Bool
any' = getAny . mconcat . map Any
      
