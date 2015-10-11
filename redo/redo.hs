{-# LANGUAGE StandaloneDeriving #-}
import Control.Monad (filterM, liftM)
import Data.Map.Lazy (fromList, insert, toList, adjust)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import System.Directory (renameFile, removeFile, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath
       (replaceBaseName, hasExtension, takeBaseName)
import System.Environment (getArgs, getEnvironment)
import System.IO (hPutStrLn, stderr)
import System.Process
       (createProcess, CreateProcess(..), waitForProcess, shell,
        StdStream(..), CmdSpec(..))

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
             case traceShow' exit of
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
          do oldEnv <- getEnvironment
             let newEnv =
                   extendEnv ("REDO_TARGET","target") oldEnv
             (_,_,_,processHandle) <-
               createProcess $
               traceShow' $
               (shell cmd) {env = Just newEnv}
             waitForProcess processHandle
        extendEnv :: (String,String) -> [(String,String)] -> [(String,String)]
        -- extendEnv item@(k,v) oldEnv =
        --   item :
        --   (filter ((/= k) . fst) oldEnv)
        extendEnv (k,v) oldEnv =
          toList $
          adjust (++ ":.") "PATH" $
          insert k v $
          fromList oldEnv
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

traceShow' arg = traceShow arg arg

-- Automatically derive an instance in places other than the data type definition point, thanks to StandaloneDeriving.
deriving instance Show CreateProcess
deriving instance Show StdStream
deriving instance Show CmdSpec
