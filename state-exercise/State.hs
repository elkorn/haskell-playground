module State where

data State s a =
  State (s -> (a,s))

get :: State s s
get = State (\s -> (s,s))

put :: s -> State s ()
put s = State (\_ -> ((),s))

modify :: (s -> s) -> State s ()
modify fn = State (\s -> ((),fn s))


instance Functor (State s) where
  -- fmap :: (a -> b) -> (State s) a -> (State s) b
  fmap fn (State st) =
    State (\s -> let (a,s2) = st s
                 in (fn a,s2))

instance Applicative (State s) where
  -- pure :: a -> (State s) a
  pure = undefined
  -- (<*>) :: (State s) (a -> b) -> (State s) a -> (State s) b
  -- How should the sequencing look like?
  (State fnSt) <*> (State st) = State (\s -> let (fn, s2) = fnSt s
                                                 (a,s3) = st s2
                                            in (fn a, s3))

instance Monad (State s) where
  -- return :: a -> (State s) a
  return = pure
  -- (>>=) :: (State s) a -> (a -> (State s) b) -> (State s) b
  (State stA) >>= fn = (State (\s -> let (a,st2) = stA s
                                         (State g) = fn a
                                     in g st2))
