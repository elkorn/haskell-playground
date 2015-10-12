{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, catchJust, IOException) -- SomeException being the root exception type.
import Control.Monad (filterM, liftM, unless, guard)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5
import Data.Map.Lazy (fromList, insert, toList, adjust)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory
       (renameFile, removeFile, doesFileExist, getDirectoryContents,
        removeDirectoryRecursive, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath
       (replaceBaseName, hasExtension, takeBaseName, (</>))
import System.Environment
       (getArgs, getEnvironment, getProgName, lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Process
       (createProcess, CreateProcess(..), waitForProcess, shell)

traceShow' :: (Show b)
           => b -> b
traceShow' arg = traceShow arg arg

redoTargetKey :: String
redoTargetKey = "REDO_TARGET"

metaDir :: String
metaDir = ".redo"

main :: IO ()
-- main = getArgs >>= mapM_ redo
main =
  do mapM_ redo =<< getArgs
     progName <- getProgName
     possibleRedoTarget <- lookupEnv redoTargetKey
     case (progName,possibleRedoTarget) of
       ("redo-ifchange",Just redoTarget) ->
         mapM_ (writeMD5 redoTarget) =<<
         getArgs
       ("redo-ifchange",Nothing) ->
         error "Missing REDO_TARGET environment variable."
       _ -> return ()
  where writeMD5 redoTarget dep =
          writeFile (metaDir </> redoTarget </> dep) =<<
          md5' dep

redo :: String -> IO ()
redo target =
  do upToDate' <-
       traceShow' `liftM`
       (upToDate metaDepsDir)
     unless upToDate' $
       maybe missingDo redo' =<<
       redoPath target
  where redo' :: FilePath -> IO ()
        redo' path =
          do recreateDirectory metaDepsDir
             (writeFile (metaDepsDir </> path)) =<<
               md5' path
             exit <- runShell $ redoCommand path
             case exit of
               ExitSuccess ->
                 do renameFile tmp target
               ExitFailure code ->
                 do removeFile tmp
                    hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++
                      show code
                    return ()
        missingDo :: IO ()
        missingDo =
          do exists <- doesFileExist target
             unless exists $ error $ "No .do file found for targets '" ++ target ++
               "'"
        redoCommand :: String -> String
        redoCommand path =
          unwords ["sh",path,"0",takeBaseName target,tmp,">",tmp]
        runShell :: String -> IO ExitCode
        runShell cmd =
          do oldEnv <- getEnvironment
             let newEnv =
                   extendEnv (redoTargetKey,target) oldEnv
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
        metaDepsDir = metaDir </> target

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target =
  listToMaybe `liftM`
  filterM doesFileExist candidates
  where candidates =
          (target ++ ".do") :
          if hasExtension target
             then [replaceBaseName target "default" ++
                   ".do"]
             else []

upToDate :: FilePath -> IO Bool
upToDate metaDepsDir =
  catch (do dependencies <- getDirectoryContents metaDepsDir
            (all id) `liftM`
              mapM depUpToDate dependencies)
        (\(_ :: IOException) -> return False)
  where
        -- return $ all $ mapM $ depUpToDate dependencies
        depUpToDate :: FilePath -> IO Bool
        depUpToDate dep =
          catch (do let path = metaDepsDir </> dep
                    oldMd5 <-
                      (head . words) -- getting the first entry in the MD5 because of the 'md5 filename' format that `md5sum` provides.
                       `liftM`
                      readFile path
                    newMd5 <- md5' dep
                    return $ oldMd5 == newMd5)
                (\e -> return $ ioeGetErrorType e == InappropriateType -- (\e -> return if ioeGetErrorType e == InappropriateType)
                                                                      -- then True -- Treat it as a correct dependency, even though it is not a dependency. It's a way of ignoring '.' and '..' directories.
                                                                      -- else False)
                 )

recreateDirectory :: FilePath -> IO ()
recreateDirectory path =
  do catchJust (\e -> guard $ isDoesNotExistError e)
               (removeDirectoryRecursive path)
               (\_ -> return ())
     createDirectoryIfMissing True path

md5' :: FilePath -> IO String
md5' path =
  (show . md5) `liftM`
  BL.readFile path
