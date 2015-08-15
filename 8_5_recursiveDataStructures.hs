-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data LL a = Empty | Cons { head :: a, tail :: LL a } deriving (Show, Read, Eq, Ord)

                                                              {-
  This is a fixity declaration.
  A fixity states how thightly an operator binds and whether it is left or right associative.
  E.g. the fixity of `*` is `infixl 7 *` and of `+` is `infixl 6 +`.
  That means they are both left-associative (1*2*3 == (1*2)*3) and `*` binds tighter than `+`.
-}
infixr 5 :-:
data LLL a = EEmpty | a :-: (LLL a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: LLL a -> LLL a -> LLL a
EEmpty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

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

main :: IO ()
main = do
  print $ 5 `Cons` Empty
  print $ Cons 12 $ Cons 5 Empty
  print $ 3 :-: 4 :-: EEmpty
  print $ 3 :-: 4 + 17 :-: EEmpty
  print $ let
    a = 3 :-: 4 :-: 5 :-: EEmpty
    b = 6 :-: 7 :-: EEmpty
    in a .++ b
  numsTree <- return $ let
    nums = [8,6,4,1,7,3,5]
    in foldr treeInsert EmptyTree nums
  print $ numsTree
  print $ 8 `treeElem` numsTree
  print $ 100 `treeElem` numsTree
