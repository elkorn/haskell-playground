{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException(..)) -- SomeException being the root exception type.
import Control.Monad (filterM, liftM, unless)
import Data.Map.Lazy (fromList, insert, toList, adjust)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import Data.Typeable (typeOf) -- for printing exception types
import GHC.IO.Exception (IOErrorType(..))
import System.Directory
       (renameFile, removeFile, doesFileExist, getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath
       (replaceBaseName, hasExtension, takeBaseName, (</>))
import System.Environment (getArgs, getEnvironment)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType)
import System.Process
       (createProcess, CreateProcess(..), waitForProcess, shell)

traceShow' arg = traceShow arg arg

main :: IO ()
-- main = getArgs >>= mapM_ redo
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target =
  do upToDate' <- upToDate target
     unless upToDate' $
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
          do oldEnv <- getEnvironment
             let newEnv =
                   extendEnv ("REDO_TARGET",target) oldEnv
             (_,_,_,processHandle) <-
               createProcess $
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

upToDate :: String -> IO Bool
upToDate target =
  catch (do dependencies <- getDirectoryContents dependenciesDir
            (traceShow' . all id) `liftM`
              mapM depUpToDate dependencies)
        (\(e :: IOException)  -> return False)
  where
        -- return $ all $ mapM $ depUpToDate dependencies
        dependenciesDir = ".redo" </> target
        depUpToDate :: FilePath -> IO Bool
        depUpToDate dep =
          catch (do oldMd5 <-
                      readFile (dependenciesDir </> dep)
                    return False)
                (\e -> return $ ioeGetErrorType e == InappropriateType)-- (\e -> return if ioeGetErrorType e == InappropriateType)
                                                                       -- then True -- Treat it as a correct dependency, even though it is not a dependency. It's a way of ignoring '.' and '..' directories.
                                                                       -- else False)
