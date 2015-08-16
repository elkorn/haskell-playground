data TrafficLight = Red | Yellow | Green

-- Making TrafficLight a typeclass instance despite that it can be derived for it.
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

{-
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  
Methods of the Eq typeclass are defined mutually 
recursively. Thanks to that we only have to 
override one of them for the typeclass instance
to work correctly.
-}

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- to define a parametric type as a typeclass instance:
-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y   = x == y
--   Nothing == Nothing = True
--   _ == _             = False

-- to see instances of a given typeclass, issue :info TypeClass in GHCi.

class TruthyFalsy a where
  truthy :: a -> Bool
  falsy :: a -> Bool
  truthy x = not $ falsy x
  falsy x = not $ truthy x

instance TruthyFalsy Int where
  truthy 0 = False
  truthy _ = True

instance TruthyFalsy [a] where
  truthy [] = False
  truthy _ = True

instance TruthyFalsy Bool where
  truthy = id

instance TruthyFalsy (Maybe a) where
  truthy (Just _) = True
  truthy Nothing = False

instance TruthyFalsy TrafficLight where
  truthy Red = False
  truthy _ = True

truthyIf :: (TruthyFalsy tf) => tf -> a -> a -> a
truthyIf cond truthyResult falsyResult = if truthy cond then truthyResult else falsyResult

main :: IO ()
main = do
  print $ Green == Red
  print $ Red `elem` [Red, Green, Yellow]
  print $ Red
  print $ truthy (0 :: Int)
  print $ truthy (1 :: Int)
  print $ truthy []
  print $ truthy Nothing
  print $ truthyIf [] "YEAH" "NOPE"
