module Internal.ColumnType (ColumnType(..), statement) where

import Data.Map as M
import Data.Maybe

data ColumnType
  = Text
  | Numeric
  | Real
  | Integer
  | Boolean
  deriving (Eq,Ord)

names :: M.Map ColumnType String
names =
  M.fromList
    [(Text,"TEXT")
    ,(Numeric,"NUMERIC")
    ,(Real,"REAL")
    ,(Integer,"INTEGER")
    ,(Boolean,"INTEGER")]

statement :: ColumnType -> String
statement =
  fromJust .
  (flip M.lookup $ names)
