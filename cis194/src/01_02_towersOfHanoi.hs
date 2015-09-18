main =
  do
    print $
      length $  hanoi 2 "a" "b" "c"
    print $
      length $  hanoi 3 "a" "b" "c"
    print $
      length $  hanoi 4 "a" "b" "c"
    print $
      length $  hanoi 5 "a" "b" "c"
    print $
      length $  hanoi 6 "a" "b" "c"

type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 =
  (hanoi (n - 1) p1 p3 p2) ++
  [(p1,p2)] ++
  (hanoi (n - 1) p3 p2 p1)
