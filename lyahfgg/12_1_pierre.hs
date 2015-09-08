main = do
  print $ landLeft 2 pole
  print $ landRight 2 pole >>= landLeft 2
  print $ Just pole >>= landLeft 1 >>= landRight 2 >>= landLeft 3
  print $ Just pole >>= landLeft 1 >>= landRight 2 >>= landLeft 6
  print $ doNotation pole

type Birds = Int
type Pole = (Birds, Birds)

pole :: Pole
pole = (0, 0)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right) 
    | otherwise = Nothing

-- landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left -(right + n)) < 4 = Just (left, right + n) 
    | otherwise = Nothing
-- landRight n (left, right) = (left, right + n)

(-:) :: a -> (a -> b) -> b
x -: f = f x

doNotation :: Pole -> Maybe Pole
doNotation pole = do
  b <- landLeft 1 pole 
  c <- landLeft 2 b
  return c
