import System.IO
import System.Directory
import Data.List

main = do
  let fileName = "todos.txt"
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todos = lines contents
      numberedTodos = zipWith (\n line -> show n ++ " - " ++ line) [0..] todos
  putStrLn "Todos:"
  -- can do `mapM putStrLn numberedTodos` as well.
  putStr $ unlines numberedTodos
  putStrLn "Which to remove?"
  numberString <- getLine
  let number = read numberString
      newTodos = delete (todos !! number) todos
  hPutStr tempHandle $ unlines newTodos
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
