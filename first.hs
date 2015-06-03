multiplyMe n x = n*x
doubleMe x = multiplyMe 2 
tripleMe x = multiplyMe 3

main = do
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
  print $ elem 2 [1,2,3]
  print $ 2 `elem` [1,2,3]
  print $ "Ranges"
  print $ [1..20]
  print $ ['A'..'z']
  print $ [1,3 .. 20]
  print $ [0.1,0.3 .. 20]
  print $ take 12 (cycle [1,2,3])
  print $ take 12 (cycle "LOL ")

