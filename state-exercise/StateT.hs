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
  fmap fn (StateT stA) =
    StateT (\s ->
              let mA = stA s
              in mA >>=
                 (\(a,s2) -> pure (fn a,s2)))

instance Monad m => Applicative (StateT s m) where
  -- pure :: a -> (StateT s m) a
  pure a = StateT (\s -> pure (a,s))
  -- (<*>) :: (StateT s m) (a -> b) -> (StateT s m) a -> (StateT s m) b
  (StateT stFn) <*> (StateT stA) =
    StateT (\s ->
              let mFn = stFn s
                  mA = stA s
              in mFn >>=
                 (\(fn,_) ->
                    mA >>=
                    (\(a,sA) -> pure (fn a,sA))))

instance Monad m => Monad (StateT s m) where
  -- return :: a -> (StateT s m) a
  return = pure
  -- (>>=) :: (StateT s m) a -> (a -> (StateT s m) b) -> (StateT s m) b
  (StateT stA) >>= fn =
    StateT (\s ->
              let mA = stA s
              in mA >>=
                 (\(a,sA) ->
                    let (StateT stB) = fn a
                    in stB sA))
