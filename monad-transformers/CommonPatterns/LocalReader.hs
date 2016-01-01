module CommonPatterns.LocalReader where

import Control.Monad.Reader (ask, local, Reader)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor.Identity (Identity)

-- When the underlying monad `m` is an instance of `MonadIO`, the `mtl` library
-- provides an instance for `ReaderT r m` and other typeclasses, such as
-- `Functor (ReaderT r m)`, `MonadIO (ReaderT r m)`, `MonadPlus (ReaderT r m)`.
myName :: String -> ReaderT String Identity String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++ "dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
