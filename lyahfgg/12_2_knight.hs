main = do
    print $
        moveKnight
            (6, 2)
    print $ in3 (6,2)
    print $ inN (6,2) 1
    print $ canReachIn (6,2) (7,3) 3
    print $ canReachIn (6,2) (7,7) 3
    print $ (6,2) `canReachIn3` (7,7)

type KnightPos = (Int, Int)
type Moves = Int

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <-
        [ (c + 2, r - 1)
        , (c + 2, r + 1)
        , (c - 2, r - 1)
        , (c - 2, r + 1)
        , (c + 1, r - 2)
        , (c + 2, r + 2)
        , (c - 1, r - 2)
        , (c - 1, r + 2)]
    guard
        (c' `elem`
         [1 .. 8] &&
         r' `elem`
         [1 .. 8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

inN :: KnightPos -> Moves -> [KnightPos]
inN start moves = let inN' positions n
                          | n == 0 =
                              positions
                          | otherwise =
                              inN'
                                  (positions >>= moveKnight)
                                  (n - 1)
    in inN' [start] moves

canReachIn :: KnightPos -> KnightPos -> Moves -> Bool
canReachIn start end moves = end `elem` inN start moves

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

class Monad m => MonadPlus m  where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m)
      => Bool -> m ()
guard True = return ()
guard False = mzero
