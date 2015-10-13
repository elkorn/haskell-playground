module MyState where

data MyState s a =
  MyState (s -> (a,s))

get :: MyState s s
get = MyState (\s -> (s,s))

put :: s -> MyState s ()
put s = MyState (\_ -> ((),s))

modify :: (s -> s) -> MyState s ()
modify fn = MyState (\s -> ((),fn s))


instance Functor (MyState s) where
  -- fmap :: (a -> b) -> (MyState s) a -> (MyState s) b
  fmap fn (MyState st) =
    MyState (\s ->
               let (a,s2) = st s
               in (fn a,s2))

instance Applicative (MyState s) where
  -- pure :: a -> (MyState s) a
  pure = undefined
  -- (<*>) :: (MyState s) (a -> b) -> (MyState s) a -> (MyState s) b
  -- How should the sequencing look like?
  (MyState fnSt) <*> (MyState st) = MyState (\s -> let
                                                 (fn, s2) = fnSt s
                                                 (a,s3) = st s2
                                            in (fn a, s3))

instance Monad (MyState s) where
  -- return :: a -> (MyState s) a
  return = pure
  -- (>>=) :: (MyState s) a -> (a -> (MyState s) b) -> (MyState s) b
  (MyState stA) >>= fn = (MyState (\s -> let (a,st2) = stA s
                                             (MyState g) = fn a
                                         in g st2))
