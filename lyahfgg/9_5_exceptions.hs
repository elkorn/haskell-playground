{-
    Exceptions can be thrown in pure as well as impure code.
    Examples: `head []` or `div 2 0`.
    However, exceptions thrown in pure code can only be handled in the I/O part.

    A general recommendation is to spend as little time as possible in the impure part of the code.
    More specific recommendation: avoid exceptions in pure code. Use `Either`s and `Maybe`s to handle potential failures through expressing them explicitly.
-}

import System.Environment
import System.Directory
import System.IO
import System.IO.Error


main = do
    withoutExceptions
    -- withExceptions

withoutExceptions :: IO ()
withoutExceptions = do
    (fileName:_) <- getArgs
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- readFile fileName
            putStrLn $
                "The file has " ++
                show (length (lines contents)) ++
                " lines."
        else do
            putStrLn "The file does not exist."

-- The packages seem to have changed in newer versions.
-- withExceptions :: IO ()
-- withExceptions = toTry `catch` errorHandler

-- toTry :: IO ()
-- toTry =do
--   (fileName:_) <- getArgs
--   contents <- readFile fileName
--   putStrLn $
--       "The file has " ++
--       show (length (lines contents)) ++
--       " lines."
-- errorHandler :: IOError -> IO ()
-- errorHandler e
--     | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--     | otherwise = ioError e
