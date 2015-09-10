import Control.Monad.State
import System.Random

main = do
    print $
        stackManip
            [0, 1, 2, 3, 4]
    print $
        runState stackManip' [0, 1, 2, 3, 4]
    print $
        runState stackStuff [1, 2, 3, 4]
    print $
        runState stackStuff [5, 2, 3, 4]
    print $
        runState moreStack [1, 2, 3, 4]
    print $
        runState moreStack [100, 2, 3, 4]
    print $
        runState stackyStack [1, 2, 3]
    print $
        runState stackyStack [1, 2, 3, 4]

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x stack = ((), x : stack)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let ((),newStack1) = push 3 stack
                       (_,newStack2) = pop newStack1
    in pop newStack2

stackManip' :: State Stack Int
stackManip' = do
    push' 3
    pop'
    pop'

-- newtype State' s a = State' { runState' :: s -> (a, s) }
-- instance Monad (State' s) where
--     return x = State' $
--         \s ->
--              (x, s)
--     (State' h) >>= f = State' $
--         \s ->
--              let (a,newState) = h s
--                  (State' g) = f a
--              in g newState

pop' :: State Stack Int
pop' = state $
    \(x:xs) ->
         (x, xs)

push' :: Int -> State Stack ()
push' n = state $
    \stack ->
         ((), n : stack)

stackStuff :: State Stack ()
stackStuff = do
    a <- pop'
    if a == 5
        then push' 5
        else do
            push' 3
            push' 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip'
  if a == 100
     then stackStuff
     else return ()

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
     then put [8,3,1]
     else put [9,2,1]


randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- The consecutive generators are being passed behind the scenes.
threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)
