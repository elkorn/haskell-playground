-- A Functor is something that can be mapped over.

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b -- `f` is a type constructor, like `Maybe`

-- instance Functor Maybe where
--   fmap f (Just x) = Just $ f x
--   fmap f Nothing  = Nothing

-- instance Functor (Either a) where
--   fmap f (Right x) = Right $ f x
--   fmap f (Left x) = Left x

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

leaf :: a -> Tree a
leaf x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = leaf x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Functor (Map k) where
  fmap f Map.Empty = Map.Empty
  fmap f ((k,v))

main :: IO ()
main = do
  print $ fmap (*2) $ Just 200
  print $ fmap (*2) $ Nothing
  print $ fmap (*2) $ foldr treeInsert EmptyTree [5,7,3,2,4,9]
