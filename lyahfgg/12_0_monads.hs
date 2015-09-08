main = do
  print $ "hello"
  print $ max <$> Just 3 <*> Just 5
  print $ applyMaybe Nothing (Just . (+1))
  print $ applyMaybe (Just 3) (Just . (+1))
  print $ (Just 3) `applyMaybe` (Just . (+1))
  print $ (Just 13) >>= (return . (*8))
  print $ (return "WHAT" :: Maybe String)
  print $ justHead "test"
  print $ justHead ""
  print $ [1,2,3] >>= \x -> [x+1,x+2,x+3]
  print $ [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
  print $ (guard (5 > 2) :: [()])
  -- `guard` facilitates a failure, making it useful for filtering.
  print $ [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

instance Monad' Maybe where
  return' = Just
  Nothing >>>= _ = Nothing
  Just x >>>= f = f x
  fail _ = Nothing

class Monad' m where
  return' :: a -> m a

  (>>>=) :: m a -> (a -> m b) -> m b

  (>>>) :: m a -> m b -> m b
  x >>> y = x >>>= \_ -> y

  fail :: String -> m a
  fail = error

justHead ::String -> Maybe Char
justHead str = do
  (h:_) <- Just str
  return h

instance Monad' [] where
  return' x = [x]
  xs >>>= f = concat (map f xs)
  fail _ = []

-- List comprehensions in terms of monads
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero
