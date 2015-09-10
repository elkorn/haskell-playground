import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

main = do
    -- liftM :: (Monad m) => (a -> b) -> m a -> m b
    -- fmap :: (Functor f) => (a -> b) -> f a -> f b
    print $
        liftM (* 3) (Just 8)
    print $
        liftM' (* 3) (Just 8)
    print $
        liftM'' (* 3) (Just 8)
    -- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
    print $
        (+) <$>
        [1, 2, 3] <*>
        [4, 5, 6]
    print $
        [(+ 1), (+ 2), (+ 3)] <*>
        [10, 11]
    print $
        [(+ 1), (+ 2), (+ 3)] `ap'`
        [10, 11]
    -- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
    print $
        liftM2
            (+)
            (Just 1)
            (Just 2)
    -- join :: (Monad m) => m (m a) -> m a
    print $
        join (Just (Just 9))
    print $
        (join (Just Nothing) :: Maybe ())
    print $
        join [[1, 2, 3], [4, 5, 6]]
    print $
        runWriter $
        join (writer (writer (1, "aaa"), "bbb"))
    print $
        (join (Right (Right 9)) :: Either () Int)
    print $
        (join (Right (Left "error")) :: Either String ())
    print $
        runState
            (join
                 (state $
                  \s ->
                       (push 10, 1 : 2 : s)))
            [10, 10, 10]
    -- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
    print $
        fst $
        runWriter $
        filterM keepSmall [1 .. 6]
    mapM_ putStrLn $
        snd $
        runWriter $
        filterM keepSmall [1 .. 6]
    print $
        powerset [1, 2, 3]
    print $
        filterM
            (\x ->
                  [odd x, even x])
            [1 .. 2]
    -- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
    print $
        foldM binSmalls 0 [2, 8, 3, 1]
    print $
        foldM binSmalls 0 [2, 8, 3, 11, 1]
-- `liftM` (`fmap`) can be implemented without referencing the Functor typeclass at all.
-- This means that monads are stronger than regular functors.
liftM' :: (Monad m)
       => (a -> b) -> m a -> m b
liftM' f m = m >>=
    (\x ->
          return $ f x)

liftM'' :: (Monad m)
        => (a -> b) -> m a -> m b
liftM'' f m = do
    x <- m
    return $ f x

-- `<*>` can also be implemented with monad stuff.
-- Thus, monads are stronger than applicatives as well.
ap' :: (Monad m)
    => m (a -> b) -> m a -> m b
ap' mf m = do
    f <- mf
    x <- m
    return $ f x

type Stack = [Int]

push :: Int -> State Stack ()
push n = state $
    \stack ->
         ((), n : stack)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large"]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM
        (\x ->
              [True, False])
        xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 =
        Nothing
    | otherwise =
        Just $ acc + x
