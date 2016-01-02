{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module EitherT where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Either (Either(..), either)

newtype EitherT l m a = EitherT
    { runEitherT :: m (Either l a)
    }

returnET :: (Monad m)
         => a -> EitherT l m a
returnET value = EitherT $ return (Right value)

failET :: (Monad m)
         => l -> EitherT l m a
failET err = EitherT $ return (Left err)

bindET :: (Monad m)
       => EitherT l m a -> (a -> EitherT l m b) -> EitherT l m b
bindET monad doNext = EitherT $
  do unwrapped <- runEitherT monad
     case unwrapped of
       Left msg -> return $ Left msg
       Right result -> runEitherT $ doNext result

instance (Monad m) => Monad (EitherT l m) where
  return = returnET
  (>>=) = bindET
  fail = failET . error

instance MonadTrans (EitherT l) where
  lift m = EitherT (Right `liftM` m)

instance (MonadIO m) => MonadIO (EitherT l m) where
  liftIO = lift . liftIO

instance (MonadState s m) => MonadState s (EitherT l m) where
  get = lift get
  put = lift . put
