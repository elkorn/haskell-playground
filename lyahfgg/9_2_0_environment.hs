import System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "Arguments: "
  mapM putStrLn args
  putStrLn "Program: "
  putStrLn progName
