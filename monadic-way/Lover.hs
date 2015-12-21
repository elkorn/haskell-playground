module Lover where
newtype Lover a = Lover
    { loverDiary :: (Name, a)
    } deriving (Show)
type Name = String
createLover :: String -> Int -> Lover Int
createLover name times = Lover (name, times)
startAffairWith :: String -> Lover Int -> Lover Int
startAffairWith name (Lover (names,times)) = Lover (name, 0)
oneMoreTime :: Lover Int -> Lover Int
oneMoreTime (Lover (name,times)) = Lover (name, times + 1)
changeBeloved :: String -> Lover Int -> Lover Int
changeBeloved newName (Lover (name,times)) = Lover (name ++ newName, times)
chainAffairs :: (Num a)
             => Lover a -> Lover a -> Lover a
chainAffairs (Lover (names,oldTimes)) (Lover (newPartner,newTimes)) = Lover
        (newPartner ++ names, newTimes + oldTimes)
times :: (a -> b) -> Lover a -> Lover b
times f (Lover (name,times)) = Lover (name, f times)
class Polygamyst f  where
    chain :: (Num a)
          => f a -> f a -> f a
instance Polygamyst Lover where
    chain partners = chainAffairs partners
instance Functor Lover where
    fmap f = times f
-- tellLover :: (Num a) => a -> a -> Lover a
tellLover newTimes oldTimes = Lover ("", newTimes + oldTimes)
-- tellMyself :: (Num a) => a -> Lover a
tellMyself n = tellLover 0 n
askLover :: Lover a -> (a -> Lover b) -> Lover b
askLover lover answer = Lover (oldNames ++ newName, newTimes)
    where (oldNames,oldTimes) = loverDiary lover
          (newName,newTimes) = loverDiary
                  (answer oldTimes)
newLove :: String -> Lover Int
newLove name = Lover (name, 0)
-- Birth and death are identical from the point of view of having lovers.
physis :: Lover Int
physis = Lover ("", 0)
dies :: Lover Int -> Lover Int
dies lover = Lover ("", 0)
-- Leonard, the guy from "Memento" - loses memory every time he wakes up.
newtype Leonard a = Leonard
    { leonardDiary :: (Name, a)
    } deriving (Show)
askLeonard lover answer = Leonard (oldNames, oldTimes)
    where (oldNames,oldTimes) = leonardDiary lover
          (newName,newTimes) = leonardDiary
                  (answer oldTimes)
tellLeonard :: Int -> Int -> Leonard Int
tellLeonard newTimes oldTimes = Leonard ("", newTimes + oldTimes)

newLeonardLove :: (Num a) => Name -> Leonard a
newLeonardLove name = Leonard (name, 0)

instance Polygamyst Leonard where
    chain (Leonard (a,b)) (Leonard _) = Leonard (a, b)


instance Functor Leonard where
  fmap f (Leonard (a, b)) = Leonard (a, f b)

instance Monad Lover where
  return a = tellMyself a
  m >>= f = askLover m f

main :: IO ()
main = do
    let jenny = startAffairWith "Jenny "
    let luisa = startAffairWith "Luisa "
    let antonia = startAffairWith "Antonia "
    let bob = createLover "Paula " 5
    let alessia = askLover
                (newLove "Joseph ")
                (tellLover 4)
    let andrea = physis
    let leonard = Leonard ("Jorja Fox", 100)
    print $
        askLover
            (tellMyself 10)
            (tellLover 1)
    print $
        askLover
            (newLove "Lory ")
            (tellLover 1)
    print $
        chain
            bob
            (chain
                 (askLover
                      (newLove "Cristal ")
                      (tellLover 2))
                 (askLover
                      (antonia bob)
                      (tellLover 4)))
    print $
        askLover bob (tellLover 10)
    print $ alessia
    print $ andrea
    print $
        askLeonard
            leonard
            (tellLeonard 2000)
    print $ fmap (+6) (chain leonard (askLeonard (newLeonardLove "Angolie ") (tellLeonard 3)))
