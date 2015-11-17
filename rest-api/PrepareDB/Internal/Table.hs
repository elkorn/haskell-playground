module Internal.Table (Table(..), create, createIfNotExists) where

import Internal.Column (Column, statement)

data Table =
  Table String
        [Column]


prettyJoin :: (Show a) => [a] -> String
prettyJoin (x:[]) = show x
prettyJoin (x:xs) = show x ++ ",\n" ++ prettyJoin xs
prettyJoin [] = ""

doCreate :: Maybe String -> Table -> String
doCreate (Just precondition) (Table tableName columns) = unwords [
  "CREATE TABLE " ,
  precondition , " " , tableName , "(\n" , (prettyJoin $ map statement columns) , ");"]
doCreate Nothing table = doCreate (Just "") table

create :: Table -> String
create = doCreate Nothing

createIfNotExists :: Table -> String
createIfNotExists = doCreate (Just "IF NOT EXISTS")
