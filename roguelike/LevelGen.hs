module LevelGen where

import Level
import Types

import qualified LevelGen.Cave as C
import qualified LevelGen.Default as D

data LevelGenerator = DefaultGenerator
                    | CaveGenerator
                      deriving Show

generateLevel :: LevelGenerator -> LevelSpec -> IO Level
generateLevel DefaultGenerator = D.generateLevel
generateLevel CaveGenerator = C.generateLevel
