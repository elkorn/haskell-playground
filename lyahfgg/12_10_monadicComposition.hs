import Control.Monad

main = do
    print $
        let f = (+ 1) . (* 100)
        in f 4
    print $
        let g = (\x ->
                      return (x + 1)) <=<
                (\x ->
                      return (x * 100))
        in Just 4 >>=
           g
    print $
        let g = (\x ->
                      return (x + 1)) <=<
                (\x ->
                      return (x * 100))
        in [4, 8] >>=
           g
    print $
        let f = foldr (.) id [(+ 1), (* 100), (+ 1)]
        in f 1

-- Referring to 12_2 examples
type KnightPos = (Int, Int)
type Moves = Int

inMany :: Moves -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Moves -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

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
