main = do
    let newTree = changeTo 'P' [R,L] freeTree
    print $ elemAt [R,L] newTree
    print $ goLeft $ goRight $ (freeTree, [])
    print $ (freeTree, []) -: goRight -: goLeft
    print $ (freeTree, []) -: goRight -: goLeft -: goUp -: goUp

data Tree a
    = Empty
    | Node a
           (Tree a)
           (Tree a)
    deriving (Show)

freeTree :: Tree Char
freeTree = Node
        'P'
        (Node
             'O'
             (Node
                  'L'
                  (Node 'N' Empty Empty)
                  (Node 'T' Empty Empty))
             (Node
                  'Y'
                  (Node 'S' Empty Empty)
                  (Node 'A' Empty Empty)))
        (Node
             'L'
             (Node
                  'W'
                  (Node 'C' Empty Empty)
                  (Node 'R' Empty Empty))
             (Node
                  'A'
                  (Node 'A' Empty Empty)
                  (Node 'C' Empty Empty)))

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving Show
type Directions = [Direction]

changeTo ::  Char -> Directions ->Tree Char -> Tree Char
changeTo c (L:ds) (Node x l r) = Node x (changeTo c ds l) r
changeTo c (R:ds) (Node x l r) = Node x l (changeTo c ds r)
changeTo c [] (Node _ l r) = Node c l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
goUp (t, []) = (t, [])

(-:) :: a -> (a -> b) -> b
x -: f = f x
