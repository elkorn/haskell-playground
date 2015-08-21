import System.Random
import Control.Monad (when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (expectedNumber,newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "What number from 1 to 10 am I thinking of?"
    numberString <- getLine
    when (not $ null numberString) $
        -- do let givenNumber = read numberString
        do let givenNumber = reads numberString -- handles incorrect input mor gracefully
           if null givenNumber
               then do
                   putStrLn "Incorrect input."
                   askForNumber newGen
               else do
                   compResult <-
                       informNumberComp
                           expectedNumber
                           (fst $ head givenNumber)
                   if compResult == True
                       then putStrLn "Fantastic!"
                       else putStrLn $ "Sorry, it was " ++ show expectedNumber

informNumberComp :: (Ord a)
                 => a -> a -> IO (Bool)
informNumberComp expected given
    | given > expected = do
        putStrLn "Too much!"
        return False
    | given < expected = do
        putStrLn "Not enough!"
        return False
    | otherwise = do
        putStrLn "Correct."
        return True
