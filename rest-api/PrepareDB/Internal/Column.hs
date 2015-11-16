module Internal.Column where

import qualified Internal.ColumnType as CT

data Column
  = Column String
           CT.ColumnType
  | IdColumn
  | PrimaryKey String
               CT.ColumnType
  | ForeignKey String -- key column name
               String -- referenced table name
               String -- referenced column name

statement :: Column -> String
statement IdColumn = "PK_ID INTEGER PRIMARY KEY AUTOINCREMENT"
statement (Column colName colType) = colName ++ " " ++ CT.statement colType
statement (PrimaryKey colName colType) = colName ++ " " ++ CT.statement colType ++
                                                           " PRIMARY KEY ASC"
statement (ForeignKey colName refTableName refColName) = "FOREIGN KEY(" ++ colName ++
                                                                           ") REFERENCES " ++
                                                                           refTableName ++
                                                                           "(" ++
                                                                           refColName ++
                                                                           ")"
