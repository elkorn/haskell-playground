import System.Random

main = do
    -- random :: (RandomGen g, Random a) => g -> (a, g)
    print $
        (random (mkStdGen 42) :: (Int, StdGen))
    print $
        threeCoins $
        mkStdGen 41232
    print $
        (take 5 $
         randoms' $
         mkStdGen 12345 :: [Int])
    print $
        (take 5 $
         randoms' $
         mkStdGen 12345 :: [Bool])
    print $
        (take 5 $
         randoms' $
         mkStdGen 12345 :: [Char])
    print $
        (take 5 $
         randoms $
         mkStdGen 12345 :: [Char])
    print $
        (finiteRandoms 5 $
         mkStdGen 12345 :: ([Char], StdGen))
    print $
        (take 10 $
         randomRs ('a', 'z') $
         mkStdGen 3 :: [Char])
    sysGen <- getStdGen
    putStrLn $
        take 20 $
        randomRs ('a', 'z') sysGen
    let randomChars = randomRs ('a', 'z') sysGen
        (first20,rest) = splitAt 20 randomChars
        (second20,_) = splitAt 20 rest
    putStrLn first20
    putStrLn second20
    sysGen' <- newStdGen
    putStrLn $
        take 20 (randomRs ('a', 'z') sysGen)
    putStrLn $
        take 20 (randomRs ('a', 'z') sysGen')
    print $
        (randomR
             (1, 100)
             (mkStdGen 1234567) :: (Int, StdGen))

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = let (first,ng) = random gen
                     (second,ng') = random ng
                     (third,_) = random ng'
    in (first, second, third)

randoms' :: (RandomGen g, Random a)
         => g -> [a]
randoms' gen = let (value,newGen) = random gen
    in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n)
              => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen = let (value,ng) = random gen
                          (tail,finalNG) = finiteRandoms
                                  (n - 1)
                                  ng
    in (value : tail, finalNG)
