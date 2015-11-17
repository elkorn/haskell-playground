module Main where

import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC

import Internal.Table

import Tables.Task (task)

tables :: [Table]
tables = [task]

createTable :: Connection -> Table -> IO Integer
createTable conn table = run conn (createIfNotExists table) []

createAll :: String -> IO ()
createAll dbName = do
  conn <- connectSqlite3 dbName
  createResult <- mapM (createTable conn) tables
  commit conn
  disconnect conn
  putStrLn $ show createResult

main :: IO ()
main = createAll "restApi.db"
