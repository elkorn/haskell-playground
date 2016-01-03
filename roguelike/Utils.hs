module Utils where

import Types

(|+|) :: Coordinates -> Coordinates -> Coordinates
(x1, y1) |+| (x2, y2) = (x1+x2,y1+y2)

(|-|) :: Coordinates -> Coordinates -> Coordinates
(x1, y1) |-| (x2, y2) = (x1-x2,y1-y2)

clamp :: Ord a => a -> a -> a -> a
clamp val _min _max = max _min $ min _max val

clampCoordinates :: Coordinates -> Coordinates -> Coordinates -> Coordinates
clampCoordinates (srcX, srcY) (minX, minY) (maxX, maxY) =
  (clamp srcX minX maxX, clamp srcY minY maxY)

-- TODO: read screen size from config
clampCoordinatesToLevel :: Coordinates -> Level -> Coordinates
clampCoordinatesToLevel coordinates (Level _ _ _ _ levelMax _ _)=
  clampCoordinates coordinates (0,0) levelMax
