module StateT where

data StateT s m a =
  StateT (s -> m (a,s))

get :: Monad m
    => StateT s m s
get = StateT (\s -> pure (s,s))

put :: Monad m
    => s -> StateT s m ()
put s = StateT (\_ -> pure ((),s))

modify :: Monad m
       => (s -> s) -> StateT s m ()
modify fn = StateT (\s -> pure ((),fn s))

instance Monad m => Functor (StateT s m) where
  -- fmap :: (a -> b) -> (StateT s m) a -> (StateT s m) b
  fmap = undefined

instance Monad m => Applicative (StateT s m) where
  -- pure :: a -> (StateT s m) a
  pure = undefined
  -- (<*>) :: (StateT s m) (a -> b) -> (StateT s m) a -> (StateT s m) b
  (<*>) = undefined

instance Monad m => Monad (StateT s m) where
  -- return :: a -> (StateT s m) a
  return = pure
  -- (>>=) :: (StateT s m) a -> (a -> (StateT s m) b) -> (StateT s m) b
  (>>=) = undefined
