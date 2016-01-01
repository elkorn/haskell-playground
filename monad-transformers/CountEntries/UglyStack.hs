{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CountEntries.UglyStack (runApp, constrainedCount) where
{-
    The result of stacking a monad transformer on a monad is another monad.
    This implies the possibility of infinite monad stacking.
    It is a common approach, as the purpose of monad transformers is to combine
    effects.

    Some reasons to create a monad stack would be:
    - Talking to the outside world requires `IO` to be at the base of the stack,
      otherwise a normal monad is enough.
    - Adding a `ReaderT` layer gives access to read-only configuration
      information.
    - Adding a `StateT` layer gives access to global state that can be modified.
    - Adding a `WriterT` layer enables the ability to log things.

    The power of the stack is that it can be customized to match the needs
    of a given use case exactly.
-}

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

import CountEntries.Classic (listDirectory)

-- import CountEntries.Shared

data AppConfig = AppConfig
    { cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState
    { stDeepestReached :: Int
    } deriving (Show)

-- `ReaderT` for storing configuration data, `StateT` for tracking the actual
-- depth.
type App = ReaderT AppConfig (StateT AppState IO) -- `ReaderT r m`

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in  runStateT (runReaderT k config) state
  --      |         |
  --      |     Removes `ReaderT` transformer wrapper.
  --  Removes `StateT` transformer wrapper.

constrainedCount :: FilePath -> App [(FilePath, Int)]
constrainedCount = constrainedCount' 0

constrainedCount' :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount' curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
       then do
         let newDepth = curDepth + 1
         state <- get
         when (stDeepestReached state < newDepth) $
           put state { stDeepestReached = newDepth } -- I don't know this notation.
         constrainedCount' newDepth newPath
       else return []
  return $ (path, length contents) : concat rest

newtype MyApp a = MyA
    { runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Monad,MonadIO,MonadReader AppConfig,MonadState AppState)
