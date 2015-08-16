import qualified Data.Set as Set
import Data.List (nub)

text1 :: String
text1 = "A long, long time ago, in a galaxy far away..."

text2 :: String
text2 = "Non scholae sed vitae discimus. Pecunia non olet."

main :: IO ()
main = do
    set1 <- return $ Set.fromList text1
    set2 <- return $ Set.fromList text2
    print $ set1
    print $ set2
    print $ Set.intersection set1 set2
    print $ Set.difference set1 set2
    print $ Set.difference set2 set1
    print $ Set.union set1 set2
    print $ Set.null Set.empty
    print $ Set.singleton 9
    print $ Set.insert 4 Set.empty
    print $ Set.insert 4 $ Set.fromList [5,3,2,1]
    print $ Set.size $ Set.insert 4 $ Set.fromList [5,3,2,1]
    print $ Set.member 1 $ Set.insert 4 $ Set.fromList [5,3,2,1]
    print $ Set.delete 4 $ Set.fromList [5,4,2,1]
    print $ Set.fromList [5,4,2,1] `Set.isSubsetOf` Set.fromList [1..5]
    print $ Set.fromList [1..5] `Set.isSubsetOf` Set.fromList [1..5]
    print $ Set.fromList [1..5] `Set.isProperSubsetOf` Set.fromList [1..5]
    print $ Set.filter odd $ Set.fromList [1..5]
    print $ Set.map (+1) $ Set.fromList [1..5]
    -- nub by Set is quicker, but List nub preserves ordering.
    print $ let setNub = Set.toList . Set.fromList
            in setNub "Heyy what is going on over here?"
    print $ nub "Heyy what is going on over here?"
