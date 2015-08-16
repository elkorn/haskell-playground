data Vector a =
    Vector a
           a
           a
    deriving (Show)

vplus :: (Num t)
      => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector
        (i + l)
        (j + m)
        (k + n)

vectMult :: (Num t)
         => Vector t -> Vector t -> Vector t
(Vector i j k) `vectMult` (Vector l m n) = Vector
        (i * l)
        (j * m)
        (k * n)

scalarMult :: (Num t)
           => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = (i * l) +
    (j * m) +
    (k * n)

main :: IO ()
main = do
    print $
        Vector 1 2 3
    print $
        (Vector 1 2 3) `vplus`
        (Vector 4 5 6)
    print $
        (Vector 1 2 3) `vectMult`
        (Vector 4 5 6)
    print $
        (Vector 1 2 3) `scalarMult`
        (Vector 4 5 6)
