import qualified Data.Map as M

data Bool   -- denoting the type
    = False               -- denoting the value constructors
    | True

data Point =
    Point Float
          Float
    deriving (Show)

data Shape
    = Circle Point
             Float
    | Rectangle Point
                Point
    deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) *
    (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle
        (Point
             (x + a)
             (y + b))
        r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle
        (Point
             (x1 + a)
             (y1 + b))
        (Point
             (x2 + a)
             (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle(Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

main :: IO ()
main = do
    print $
        surface $
        Circle (Point 10 20) 10
    print $
        surface $
        Rectangle
            (Point 1 2)
            (Point 3 4)
    print $
        Circle (Point 10 20) 10
    print $
        Rectangle
            (Point 1 2)
            (Point 3 4)
    print $
        map
            (Circle (Point 10 20))
            [1 .. 5] -- geocentric circles
    print $
        nudge (Circle (Point 10 20) 15) 5 10
    print $ nudge (baseRect 40 100) 60 23
