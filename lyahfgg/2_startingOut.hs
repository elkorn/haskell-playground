multiplyMe n x = n*x
doubleMe x = multiplyMe 2 
tripleMe x = multiplyMe 3


-- run a = const $ return $ unlines $ reload ++ a 

-- reload = [":reload"]
-- actions = ["basic", "comprehensions", "tuples"]

-- hoge = run actions

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum[1 | _ <- xs]
removeNonUppercase str = [char | char <- str, char `elem` ['A'..'Z']]
getEvenFromLists xxs = [[x | x<- xs, even x] | xs <- xxs]
triangles cs = [(a,b,c) | c <- cs, b <- [1..c], a <- [1..b]]
rightTriangles cs = [(a,b,c)| (a,b,c) <- triangles cs,a^2+b^2==c^2]
rightTrianglesForPerimiter cs perimiter = [(a,b,c) | (a,b,c) <- rightTriangles cs, a+b+c==perimiter]

tuples = do
    print $ "TUPLES"
    print $ ("Wow", False)
    print $ fst ("Wow", False)
    print $ snd ("Wow", False)
    print $ zip [1,2,3,4] ['a','b','c','d']
    print $ zip [1,2,3,4] [5,6]
    print $ zip [1..] ['a','b','c']
    print $ triangles [1..10]
    print $ rightTriangles [1..10]
    print $ rightTrianglesForPerimiter [1..20] 24

comprehensions = do
  print $ "COMPREHENSIONS"
  print $ [x*2 | x <- [1..10]]
  print $ [x*2 | x <- [1..10], x*2 >= 12]
  print $ [x*2 | x <- [50..100], x`mod`7 == 3]
  print $ boomBangs [1..20]
  print $ "Several predicates"
  print $ [x*2 | x <- [50..100], x`mod`7 == 3, x /= 52]
  print $ "Drawing from several lists"
  print $ [x*2 | x <- [2,4,6], y <- [3,5,7]]
  print $ "Custom length fn"
  print $ length' [1..142]
  print $ "Removing non-uppercase"
  print $ removeNonUppercase "Test OF removing Non-Upper-Case"
  print $ "Nested comprehension"
  print $ getEvenFromLists [[1,2,3], [2,4,6], [3,5,7]]


basic = do
  print $ "BASICS"
  print $ "Prepending"
  print $ 'A':' ':"SMALL CAT"
  print $ "List comparisons (lex)"
  print $ [1,2,3] > [0,1,4]
  print $ [1,2,3] > [2,0,0]
  print $ "List fns"
  print $ head [1,2,3]
  print $ tail [1,2,3]
  print $ last [1,2,3]
  print $ init [1,2,3]
  print $ length [1,2,3]
  print $ null [1,2,3]
  print $ null []
  print $ reverse [1,2,3]
  print $ take 2 [1,2,3]
  print $ drop 2 [1,2,3]
  print $ sum [1,2,3]
  print $ product [1,2,3]
  print $ elem 2 [1,2,3] -- tells if 2 is an element of [1,2,3]
  print $ 2 `elem` [1,2,3]
  print $ "Ranges"
  print $ [1..20]
  print $ ['A'..'z']
  print $ [1,3 .. 20]
  print $ [0.1,0.3 .. 20]
  print $ take 12 (cycle [1,2,3])
  print $ take 12 (cycle "LOL ")
  print $ take 10 (repeat 5)

main = do
  basic
  comprehensions
  tuples
