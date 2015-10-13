module MyStateT where

data MyStateT s m a =
  MyStateT (s -> m (a,s))

get :: Monad m
    => MyStateT s m s
get = MyStateT (\s -> pure (s,s))

put :: Monad m
    => s -> MyStateT s m ()
put s = MyStateT (\_ -> pure ((),s))

modify :: Monad m
       => (s -> s) -> MyStateT s m ()
modify fn = MyStateT (\s -> pure ((),fn s))

instance Monad m => Functor (MyStateT s m) where
  -- fmap :: (a -> b) -> (MyStateT s m) a -> (MyStateT s m) b
  fmap = undefined

instance Monad m => Applicative (MyStateT s m) where
  -- pure :: a -> (MyStateT s m) a
  pure = undefined
  -- (<*>) :: (MyStateT s m) (a -> b) -> (MyStateT s m) a -> (MyStateT s m) b
  (<*>) = undefined

instance Monad m => Monad (MyStateT s m) where
  -- return :: a -> (MyStateT s m) a
  return = pure
  -- (>>=) :: (MyStateT s m) a -> (a -> (MyStateT s m) b) -> (MyStateT s m) b
  (>>=) = undefined
