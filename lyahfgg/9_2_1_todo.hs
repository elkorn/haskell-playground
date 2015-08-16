import System.Environment
import System.Directory
import System.IO
import Data.List

type Arguments = [String]
type Todos = [String]
type Name = String
type Action = Arguments -> IO ()
type Command = (Name, Action)

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

dispatch :: [Command]
dispatch = [ ("add", add)
           , ("remove", remove)
           , ("view", view)
           , ("bump", bump)
           ]

add :: Action
add [fileName, todo] = appendFile fileName $ todo ++ "\n"

view :: Action
view [fileName] = do
  contents <- readFile fileName
  let todos = lines contents
      numberedTodos = zipWith (\n todo -> show n ++ " - " ++ todo) [0..] todos
  putStr $ unlines numberedTodos

remove :: Action
remove = updateFile (\todos number -> delete (todos !! number) todos)

bump :: Action
bump = updateFile (\todos number -> (todos !! number) : (take number todos) ++ (drop (number + 1) todos))


type TodosTransformer = (Todos -> Int -> Todos)
updateFile :: TodosTransformer -> Action
updateFile todosTransformer = (\[fileName, numberString] -> do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todos = lines contents
      newTodos = todosTransformer todos number
  hPutStr tempHandle $ unlines newTodos
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName)
