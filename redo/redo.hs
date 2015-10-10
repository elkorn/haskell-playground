import Control.Monad (filterM, liftM)
import Data.Maybe (listToMaybe)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath
       (replaceBaseName, hasExtension, takeBaseName)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)

main :: IO ()
-- main = getArgs >>= mapM_ redo
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target =
  maybe printMissing redo' =<<
  redoPath target
  where redo' :: FilePath -> IO ()
        redo' path =
          do exit <- runShell $ redoCommand path
             case exit of
               ExitSuccess ->
                 do renameFile tmp target
               ExitFailure code ->
                 do removeFile tmp
                    hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++
                      show code
                    return ()
        printMissing :: IO ()
        printMissing = error $ "No .do file found for targets '" ++ target ++
                                                                    "'"
        redoCommand :: String -> String
        redoCommand path =
          unwords ["sh",path,"0",takeBaseName target,tmp,">",tmp]
        runShell :: String -> IO ExitCode
        runShell cmd =
          do (_,_,_,processHandle) <- createProcess $ shell cmd
             waitForProcess processHandle
        tmp = target ++ "---redoing"

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target =
  listToMaybe `liftM`
  filterM doesFileExist candidates
  where candidates =
          [target ++ ".do"] ++
          if hasExtension target
             then [replaceBaseName target "default" ++
                   ".do"]
             else []
