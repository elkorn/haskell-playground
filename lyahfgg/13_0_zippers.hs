main = do
    let newTree = changeTo 'P' [R,L] freeTree
    print $ elemAt [R,L] newTree
    -- print $ goLeft $ goRight $ (freeTree, [])
    print $ return (freeTree, []) >>= goRight >>= goLeft
    print $ return (freeTree, []) >>= goRight >>= goLeft >>= goUp >>= goUp
    -- print $ (modify (\_ -> 'X') $ (freeTree, []) >>= goLeft >>= goRight) >>= goUp >>= goUp
    -- print $ "---"
    print $ return (freeTree, []) >>= goLeft >>= goRight >>= modify (\_ -> 'X') >>= goUp >>= goUp
    print $ let farLeft = return (freeTree, []) >>= goLeft >>= goLeft >>= goLeft >>= goLeft
      in  farLeft >>= topMost >>= attach (leaf 'Z')

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
                  (leaf 'N')
                  (leaf 'T'))
             (Node
                  'Y'
                  (leaf 'S')
                  (leaf 'A')))
        (Node
             'L'
             (Node
                  'W'
                  (leaf 'C')
                  (leaf 'R'))
             (Node
                  'A'
                  (leaf 'A')
                  (leaf 'C')))

leaf :: a -> Tree a
leaf x = Node x Empty Empty

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

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

modify :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (Node x l r, bs) = Just (Node (f x) l r, bs)
modify f (Empty, bs) = Just (Empty, bs)

attach :: Tree a -> Zipper a -> Maybe (Zipper a)
attach t (_, bs) = Just (t, bs)

topMost :: Zipper a -> Maybe (Zipper a)
topMost (t, []) = Just (t, [])
topMost z = goUp z >>= topMost
-- topMost z = do
--   up <- goUp z
--   topMost up
