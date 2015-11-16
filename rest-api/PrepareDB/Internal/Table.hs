module Internal.Table where

import Internal.Column (Column, statement)

data Table =
  Table String
        [Column]

create :: Table -> String
create (Table tableName columns) = "CREATE TABLE " ++ tableName ++ "(\n" ++ (unlines $ map (\col -> (statement col )++ ",") columns) ++ ");"
