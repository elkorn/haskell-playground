{-
    Input format:
    10            -> road A
    20            -> road B
    5             -> crossing road
    17            -> road A
    9             -> road B  
    4             -> crossing road
    ...
-}
main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadNetwork = map (\[a,b,c] -> Section a b c) threes
        (path, pathCost) = optimalPath roadNetwork
        pathString = concat $ map (show . fst) path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Its cost is: " ++ show pathCost ++ " minutes"
    -- let p = optimalPath heathrowToLondon
    -- print p
    -- print $
    --     p ==
    --     [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8), (C, 0)]

data Section = Section
    { getA :: Int
    , getB :: Int
    , getC :: Int
    } deriving (Show)

type RoadNetwork = [Section]

heathrowToLondon :: RoadNetwork
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0]

data Label
    = A
    | B
    | C
    deriving (Show,Eq)
type Path = [(Label, Int)]

optimalPath :: RoadNetwork -> (Path, Int)
optimalPath roadNetwork = let (bestAPath,bestCostToA,bestBPath,bestCostToB) = foldl
                                      roadStep
                                      ([], 0, [], 0)
                                      roadNetwork
    in if bestCostToA <= bestCostToB
           then (reverse bestAPath, bestCostToA)
           else (reverse bestBPath, bestCostToB)

roadStep :: (Path, Int, Path, Int) -> Section -> (Path, Int, Path, Int)
roadStep (pathA,costA,pathB,costB) (Section a b c) = let forwardCostToA = costA +
                                                             a
                                                         crossCostToA = costB +
                                                             b + c
                                                         forwardCostToB = costB +
                                                             b
                                                         crossCostToB = costA +
                                                             a + c
                                                         (newPathToA,newCostToA) = if forwardCostToA <=
                                                                                      crossCostToA
                                                                 then ( (A, a) :
                                                                        pathA
                                                                      , forwardCostToA)
                                                                 else ( (C, c) :
                                                                        (B, b) :
                                                                        pathB
                                                                      , crossCostToA)
                                                         (newPathToB,newCostToB) = if forwardCostToB <=
                                                                                      crossCostToB
                                                                 then ( (B, b) :
                                                                        pathB
                                                                      , forwardCostToB)
                                                                 else ( (C, c) :
                                                                        (A, a) :
                                                                        pathA
                                                                      , crossCostToB)
    in (newPathToA, newCostToA, newPathToB, newCostToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
