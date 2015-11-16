module Tables.Task where

import qualified Internal.ColumnType as CT
import Internal.Column
import Internal.Table

task :: Table
task =
  Table "Task" [IdColumn,Column "NAME" CT.Text,Column "STATUS" CT.Boolean]
