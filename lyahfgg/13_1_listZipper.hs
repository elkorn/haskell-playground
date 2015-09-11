main = do
  let xs = [1..4]
  print $ goForward (xs, [])
  print $ goBack $ goForward $ goForward (xs, [])

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)
