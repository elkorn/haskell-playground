{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module MaybeT where

import Control.Monad 
import Control.Monad.State
import Control.Monad.Trans

import Data.Maybe (Maybe(..), maybe)

newtype MaybeT m a = MaybeT
    { runMaybeT :: m (Maybe a)
    }

-- Everything inside the `do` block executes in the underlying monad `m`,
-- whatever it might be.
bindMT :: (Monad m)
       => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT monad doNext = MaybeT $
    do unwrapped <- runMaybeT monad
       case unwrapped of
           Nothing -> return Nothing
           Just value -> runMaybeT $ doNext value

idiomaticBindMT :: (Monad m)
                => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
idiomaticBindMT monad doNext = MaybeT $ runMaybeT monad >>=
    maybe
        (return Nothing)
        (runMaybeT . doNext)


returnMT :: (Monad m) => a -> MaybeT m a
returnMT value = MaybeT $ return (Just value)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=) = idiomaticBindMT
    fail = failMT
-- The underlying monad is of type `m a`, we wrap it with the `Just` ctor so
-- that it fits into `MaybeT` and then hide the result inside a `MaybeT`.
instance MonadTrans MaybeT where
  lift m = MaybeT (Just `liftM` m)

-- Once a `MonadTrans` instance is defined, we can use `MaybeT` to define
-- instances other `mtl` typeclasses.
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO 

-- requires FlexibleInstances, MultiParamTypeclasses and UndecidableInstances.
instance (MonadState s m) => MonadState s (MaybeT m) where 
  get = lift get
  put = lift . put

-- main :: IO ()
-- main = do
--   let just1 = returnMT 1
--   let just2 = returnMT 2
--   let nada = failMT 3 
--   one <- just1 >>= liftIO
--   -- two <- liftIO just2
--   -- print $ one + two
--   print "HI"
