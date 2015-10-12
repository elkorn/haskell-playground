{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, catchJust, IOException) -- SomeException being the root exception type.
import Control.Monad (filterM, liftM, unless, guard)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Map.Lazy (fromList, insert, toList, adjust)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory
       (renameFile, removeFile, doesFileExist, getDirectoryContents,
        removeDirectoryRecursive, createDirectoryIfMissing,
        getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath
       (replaceBaseName, hasExtension, takeBaseName, (</>), splitFileName)
import System.Environment
       (getArgs, getEnvironment, getProgName, lookupEnv)
import System.IO
       (hPutStrLn, stderr, IOMode(..), withFile, hFileSize)
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
  do mapM_ redoWithDirectorySupport =<< getArgs
     progName <- getProgName
     possibleRedoTarget <- lookupEnv redoTargetKey
     case (progName,possibleRedoTarget) of
       ("redo-ifchange",Just redoTarget) ->
         mapM_ (writeMD5 redoTarget) =<<
         getArgs
       ("redo-ifchange",Nothing) ->
         error "Missing REDO_TARGET environment variable."
       _ -> return ()
  where redoWithDirectorySupport :: String -> IO ()
        redoWithDirectorySupport target =
          do topDir <- getCurrentDirectory
             let (dir,file) = splitFileName target
             setCurrentDirectory dir
             redo file
             setCurrentDirectory topDir

redo :: String -> IO ()
redo target =
  do upToDate' <- (upToDate target)
     unless upToDate' $
       maybe missingDo redo' =<<
       doPath target
  where redo' :: FilePath -> IO ()
        redo' path =
          do recreateDirectory metaDepsDir
             writeMD5 target path
             -- (writeFile (metaDepsDir </> path)) =<<
             --   fileMd5 path
             exit <- runShell $ redoCommand path
             case exit of
               ExitSuccess ->
                 do size <- fileSize tmp
                    if size > 0
                       then renameFile tmp target
                       else removeFile tmp -- phony target handling
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
          unwords ["sh -x",path,"0",takeBaseName target,tmp,">",tmp]
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

doPath :: FilePath -> IO (Maybe FilePath)
doPath target =
  listToMaybe `liftM`
  filterM doesFileExist candidates
  where candidates =
          (target ++ ".do") :
          if hasExtension target
             then [replaceBaseName target "default" ++
                   ".do"]
             else []

upToDate :: FilePath -> IO Bool
upToDate target =
  catch (do exists <- doesFileExist target
            if exists
               then do md5s <- getDirectoryContents $ metaDir </> target
                       and `liftM`
                         mapM depUpToDate md5s
               else return False)
        (\(_ :: IOException) -> return False)
  where
        -- return $ all $ mapM $ depUpToDate dependencies
        depUpToDate :: String -> IO Bool
        depUpToDate oldMd5 =
          catch (do let path = metaDir </> target </> oldMd5
                    dep <-
                           -- (head . words) -- getting the first entry in the MD5 because of the 'md5 filename' format that `md5sum` provides.
                           --  `liftM`
                           readFile path
                    newMd5 <- fileMd5 dep
                    doScript <- doPath dep
                    case doScript of
                      Nothing -> return $ oldMd5 == newMd5
                      Just _ ->
                        do upToDate' <- upToDate dep
                           return $
                             (oldMd5 == newMd5) &&
                             upToDate')
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

fileMd5 :: FilePath -> IO String


fileMd5 path =
  (show . MD5.md5) `liftM`
  BL.readFile path
writeMD5 :: String -> FilePath -> IO ()
writeMD5 redoTarget dep =
  do md5 <- fileMd5 dep
     writeFile (metaDir </> redoTarget </> md5) dep

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
