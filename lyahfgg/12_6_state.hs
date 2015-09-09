main = do
  print $ stackManip [0]

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x stack = ((), x:stack)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((), newStack1) = push 3 stack
  (_, newStack2) = pop newStack1
  in pop newStack2

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = do
  push 3
  a <- pop
  pop


newtype State' s a = State' { runState' :: s -> (a, s) }

instance Monad (State' s) where
  return x = State' $ \s -> (x, s)
  (State' h) >>= f = State' $ (\s -> let (a, newState) = h s
                                      (State' g) = f a
                                    in g newState)

