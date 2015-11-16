module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

import Internal.Table

import Tables.Task (task)

createAll :: String -> IO ()
createAll dbName = do
  -- conn <- connectSqlite3 dbName
  putStrLn $ create task


main :: IO ()
main = createAll "restApi.db"
