import System.IO
import qualified Data.Char as Char

main = do
  -- openFile :: FilePath -> IOMode -> IO Handle
  -- `FilePath` is a synonym for `String`.
  -- `IOMode` is an enum:
  -- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
  handle <- openFile "data.txt" ReadMode
  -- hGetContents :: Handle -> IO String
  contents <- hGetContents handle
  putStr contents
  hClose handle
  withFile "data.txt" ReadMode (\handle -> do
                                    contents <- hGetContents handle
                                    putStr contents)
  withFile' "data.txt" ReadMode (\handle -> do
                                    contents <- hGetContents handle
                                    putStr contents)
  -- there also exist file counterparts to 'normal' IO functions:
  -- `hGetLine`, `hPutstr`, `hPutStrLn`, `hGetChar` etc.
  -- It is worth noting that files are streams as well.
  -- Disk access for text files is line-buffered, for binary files chunk-buffered.
  d <- readFile "data.txt"
  print d
  writeFile "datacaps.txt" $ map Char.toUpper d
  addTodo "Learn some Haskell"
  addTodo "Write a project in Haskell"
  -- Reading in big chunks can help to minimize disk access or when the 'file' is actually a
  -- slow network resource.
  withFile "data.txt" ReadMode (\handle -> do
                                    hSetBuffering handle $ BlockBuffering $ Just 2048
                                    contents <- hGetContents handle
                                    putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode fn = do
  handle <- openFile path mode
  result <- fn handle
  hClose handle
  return result

addTodo :: String -> IO ()
addTodo todo = do
  appendFile "todos.txt" $ todo++ "\n"
