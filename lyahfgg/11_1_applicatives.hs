main = do
  -- The function being `fmap`ped gets partially applied with the second argument.
  -- The result is a functor containing that function partially applied to the contents of the previous functor.
  let a = fmap (*) [1..4]
  print $ fmap (\f -> f 12) a
  print $ fmap (\f -> f 12) $ fmap (*) [1..4]
  print $ fmap (\f -> f 'A') $ fmap compare "A LIST OF CHARS"
  print $ fmap (\f -> f 12) $ fmap (*) (Just 12)
  -- Functor instances allow only mapping normal functions over functors.
  -- To do something like `fmap (Just (3 *)) (Just 5)`, we need applicative functors.
  print $ Just (+3) <*> (Just 9)
  print $ pure (+3) <*> (Just 9)
  -- print $ Nothing <*> (Just 5)
  print $ Just (+5) <*> Nothing
  -- The feature that is not available in functors is chaining
  print $ pure (+) <*> Nothing <*> Just 5
  -- A function that takes normal parameters can be lifted to the domain of any applicative functor.
  print $ pure (+) <*> Just 13 <*> Just 5
  print $ fmap (+) (Just 13) <*> Just 5
  -- `<$>` is an infix `fmap` alias.
  print $ (+) <$> (Just 13) <*> Just 5
  print $ (pure "Hey":: [String])
  print $ (pure "Hey":: Maybe String)
  print $ [(*10), (+17), (^2)] <*> [1,2,3]
  -- [(1+), (2+), (1*), (2*)] <*> [3,4], due to left-associativity of <*>
  print $ [(+), (*)] <*> [1,2] <*> [3,4]
