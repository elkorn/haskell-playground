import Control.Monad.Error

main = do
  print $ Left "boom" >>= \x -> return (x+1)
  print $ ((Right 12) >>= \x -> return (x+1) :: Either () Int)

-- instance (Error e) => Monad (Either e) where
--   return x = Right x
--   Right x >>= f = f x
--   Left err >>= _ = Left err
--   fail = Left . strMsg 
