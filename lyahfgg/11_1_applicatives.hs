import Control.Applicative

main = do
    -- The function being `fmap`ped gets partially applied with the second argument.
    -- The result is a functor containing that function partially applied to the contents of the previous functor.
    let a = fmap (*) [1 .. 4]
    print $
        fmap
            (\f ->
                  f 12)
            a
    print $
        fmap
            (\f ->
                  f 12) $
        fmap (*) [1 .. 4]
    print $
        fmap
            (\f ->
                  f 'A') $
        fmap compare "A LIST OF CHARS"
    print $
        fmap
            (\f ->
                  f 12) $
        fmap (*) (Just 12)
    -- Functor instances allow only mapping normal functions over functors.
    -- To do something like `fmap (Just (3 *)) (Just 5)`, we need applicative functors.
    print $
        Just (+ 3) <*>
        (Just 9)
    print $
        pure (+ 3) <*>
        (Just 9)
    -- print $ Nothing <*> (Just 5)
    print $
        Just (+ 5) <*>
        Nothing
    -- The feature that is not available in functors is chaining
    print $
        pure (+) <*> Nothing <*>
        Just 5
    -- A function that takes normal parameters can be lifted to the domain of any applicative functor.
    print $
        pure (+) <*>
        Just 13 <*>
        Just 5
    print $
        fmap (+) (Just 13) <*>
        Just 5
    -- `<$>` is an infix `fmap` alias.
    print $
        (+) <$>
        (Just 13) <*>
        Just 5
    print $
        pure (+) <*>
        (Just 13) <*>
        Just 5
    print $
        (pure "Hey" :: [String])
    print $
        (pure "Hey" :: Maybe String)
    print $
        [(* 10), (+ 17), (^ 2)] <*>
        [1, 2, 3]
    -- [(1+), (2+), (1*), (2*)] <*> [3,4], due to left-associativity of <*>
    print $
        [(+), (*)] <*>
        [1, 2] <*>
        [3, 4]
    -- x <- (++) <$> getLine <*> getLine
    -- print $ x
    print
        [x * y | x <- [2, 5, 10]
               , y <- [8, 10, 11]]
    print $
        (*) <$>
        [2, 5, 10] <*>
        [8, 10, 11]
    -- functions are also applicatives.
    print $ (+) <$> (+ 3) <*> (* 100) $ 5
    -- f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    print $
        (\x y z ->
              [x, y, z]) <$>
        (+ 3) <*>
        (* 2) <*>
        (/ 2) $
        5
    print $ (+) <$> (+2) <*> (+18) $ 1
    print $ getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100, 100, 100]
    print $ getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100..]
    print $ getZipList $ (,,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat" <*> ZipList "fog"
    print $ getZipList $ (\w x y z -> (w,x,y,z)) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat" <*> ZipList "fog"
    -- `liftA2` promotes a binary function to operate on two functors.
    print $ liftA2 (+) (*3) (+8) 12
    -- creates a function that takes a parameter to which both following functions will be applied separately. The final result will be merged by calling the first function on the aforementioned results.
    print $ (+) <$> (*3) <*> (+8) $ 12
    print $ fmap (\x -> [x]) (Just 4)
    print $ liftA2 (:) (Just 3) (Just [4])
    print $ (++) <$> ((:) <$> (Just 3) <*> (Just [4])) <*> (Just [2])
    print $ sequenceA $ map (Just) [1..10]
    print $ sequenceA' $ map (Just) [1..10]
    print $ sequenceA'' $ map (Just) [1..10]
    print $ sequenceA' $ map (Just) [1..10]
    print $ sequenceA' $ [Just 1, Nothing, Just 3]
    print $ sequenceA [(2+), (*3), (+13)] 12
    -- useful
    print $ map (\f -> f 7) [(>4), (<10), odd]
    print $ sequenceA [(>4), (<10), odd] 7
    print $ and $ map (\f -> f 7) [(>4), (<10), odd]
    print $ sequenceA [(>4), (<10), odd] 7
    print $ sequenceA [[1,2,3], [4,5,6]]
    -- (:) <$> [1,2] <*> sequenceA [[3, 4]]
    -- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [[]])
    -- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [])
    -- (:) <$> [1,2] <*> [3:[], 4:[]]
    -- [1:3:[], 1:4:[], 2:3:[], 2:4:[]]
    print $ sequenceA [[1,2], [3,4]]
    print $ (+) <$> [1,2] <*> [4,5,6]
    print $ sequenceA [[1,2], [3,4], [5,6], [7,8]]
    x <- sequenceA [getLine, getLine, getLine]
    print x
    -- Applicative laws
    -- pure id <*> v = v
    -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- pure f <*> pure x = pure (f x)
    -- u <*> pure y = pure ($ y) <*> u

-- Allows chaining arbitrary numbers of applicatives together.
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])
