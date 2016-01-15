module Sudoku where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.List
-- Non-monad ready
-- move :: SudokuState -> Move -> SudokuState
-- move = undefined
-- moves :: SudokuState -> [Move] -> SudokuState
-- moves = foldl move
-- Monad ready
executeMove :: Move -> SudokuState ()
executeMove ((x,y),value) = update x y value

executeMoves :: [Move] -> SudokuState ()
executeMoves = mapM_ executeMove

type StatePlus s a = StateT s Maybe a
type SudokuGrid = [[Int]]
type Position = (Int, Int)
type Move = (Position, Int)

type SudokuState a = StatePlus SudokuGrid a

gridRange :: [Int]
gridRange = [0 .. 8]

times :: Int -> a -> [a]
times n v = take n $ repeat v

emptyRow :: [Int]
emptyRow = times 9 0

initialGrid :: SudokuGrid
initialGrid = times 9 emptyRow

getRows :: SudokuState [[Int]]
getRows = gets id

getRow :: Int -> SudokuState [Int]
getRow index = do
    rows <- getRows
    return $ rows !! index

selectRows :: [[a]] -> [Int] -> [[a]]
selectRows values range = map (values !!) range

selectColumns :: [[a]] -> [Int] -> [[a]]
selectColumns values range = map (selectColumn values) range
    where selectColumn :: [[a]] -> Int -> [a]
          selectColumn values n = map (flip (!!) n) values

getColumns :: SudokuState [[Int]]
getColumns = gets $
    flip selectColumns gridRange

getColumn :: Int -> SudokuState [Int]
getColumn index = do
    columns <- getColumns
    return $ columns !! index
{-
    ### ### ###
    ### ### ###
    ### ### ###

    ### ### ###
    ### ### ###
    ### ### ###

    ### ### ###
    ### ### ###
    ### ### ###
-}
getSubgrids :: SudokuState [[Int]]
getSubgrids = gets $
    \values ->
         map
             (\(rows,columns) ->
                   selectSubgrid values rows columns)
             [ ([0 .. 2], [0 .. 2])
             , ([0 .. 2], [3 .. 5])
             , ([0 .. 2], [6 .. 8])
             , ([3 .. 5], [0 .. 2])
             , ([3 .. 5], [3 .. 5])
             , ([3 .. 5], [6 .. 8])
             , ([6 .. 8], [0 .. 2])
             , ([6 .. 8], [3 .. 5])
             , ([6 .. 8], [6 .. 8])]
  where
    selectSubgrid :: [[a]] -> [Int] -> [Int] -> [a]
    selectSubgrid values rows columns = concat $
        selectColumns
            (selectRows values rows)
            columns
getSubgrid :: Int -> SudokuState [Int]
getSubgrid index = do
    subgrids <- getSubgrids
    return $ subgrids !! index
getCellValue :: Int -> Int -> SudokuState Int
getCellValue row column = gets $
    (\values ->
          values !! row !! column)
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]
getAllCellValues :: SudokuState [(Position, Int)]
getAllCellValues = do
    rows <- get
    return $
        concatMap
            (\(y,row) ->
                  zip
                      (zipWithIndex $
                       times 9 y)
                      row) $
        zipWithIndex rows
replace :: Int -> a -> [a] -> [a]
replace index value list = map
        (\(i,v) ->
              if i == index
                  then value
                  else v) $
    zipWithIndex list
-- `modify` analogue for the sudoku state transformer
update :: Int -> Int -> Int -> SudokuState ()
update x y value = modify $
    \rows ->
         replace y (replace x value (rows !! y)) rows
-- Move format: 'A1:4'
--               ||  \
--               |\   value to place at coordinates
--               \ column
--                row
makeMove :: String -> (Position, Int)
makeMove (row:column:_:value:_) = ( (parseChar row, parseInt column)
                                  , parseInt value)
    where simpleParser :: [Char] -> Char -> Int
          simpleParser domain c = let (Just n) = elemIndex c domain
              in n
          parseChar :: Char -> Int
          parseChar = simpleParser
                  ['A' .. 'I']
          parseInt :: Char -> Int
          parseInt = simpleParser
                  ['1' .. '9']
getEmptyPositions :: SudokuState [Position]
getEmptyPositions = do
    allCellValues <- getAllCellValues
    return $
        map fst .
        filter
            (\(_,v) ->
                  v == 0) $
        allCellValues

initialize  :: [String] -> SudokuState ()
initialize = executeMoves . map makeMove

containingSubgrid :: Position -> Int
containingSubgrid (x,y) = (x `div` 3) +
    (3 *
     (y `div` 3))

getPermittedValues :: Position -> SudokuState [Int]
getPermittedValues position@(x,y) = do
    row <- getRow y
    column <- getColumn x
    subgrid <- getSubgrid $ containingSubgrid position
    return $
        [1 .. 9] \\
        (row ++ column ++ subgrid)

getPossibleMoves :: SudokuState [(Position, [Int])]
getPossibleMoves = do
  emptyPositions <- getEmptyPositions
  mapM (\position -> do
           permittedValues <- getPermittedValues position
           return (position, permittedValues)) emptyPositions

atom :: (Position, [Int]) -> Bool
atom (_,[]) = True
atom _ = False

-- Forced moves are the ones without possible alternatives.
getForcedMoves :: SudokuState [Move]
                  
getForcedMoves = do
  possibleMoves <- getPossibleMoves
  let forcedMoves = filter (atom) possibleMoves
  return $ [(pos, v)| (pos, (v:_)) <- forcedMoves]

executeForcedMoves :: SudokuState ()
executeForcedMoves = do
  forcedMoves <- getForcedMoves
  if null forcedMoves
     then return ()
     else executeMoves forcedMoves >> executeForcedMoves

getNextMove :: SudokuState [Move]
getNextMove = do
  possibleMoves <- getPossibleMoves
  let blocked = filter (null . snd) possibleMoves
  if null possibleMoves
     then return []
     else if null blocked
             then return $ fewestOptions possibleMoves
             else mzero

-- fewestOptions :: [(Position, [a])] -> (Position, [a])
fewestOptions es =
  let (e:_) = sortBy (\(_,cs1) (_,cs2) -> compare (length cs1) (length cs2)) es
      (pos, cs) = e
   in [(pos,c) | c <- cs]

solve = do
  executeForcedMoves
  valid <- checkValid
  if valid
     then do
        nextMove <- getNextMove
        if null nextMove
           then return ()
           else msum . map (\move -> executeMove move >> solve) $ nextMove
        else mzero

checkValid :: SudokuState Bool
checkValid = do
  rows <- getRows
  columns <- getColumns
  subgrids <- getSubgrids
  let valid = foldr (\b x -> x && (allValid b)) True [rows, columns, subgrids] 
  return True

allValid :: [[Int]] -> Bool
allValid = foldr (\x b -> b && (noDups x)) True

noDups :: [Int] -> Bool
noDups [] = True
noDups (x:xs) = (if x == 0 then True else not (x `elem` xs)) && noDups xs

prettyPrint :: [[Int]] -> IO ()
prettyPrint rows = mapM_ prettyPrintRow rows >> putStr "\n"

prettyPrintRow :: [Int] -> IO ()
prettyPrintRow row = mapM_ prettyPrintPosition row >> putStr "\n"

prettyPrintPosition :: Int -> IO ()
prettyPrintPosition 0 = putStr "."
prettyPrintPosition p = putStr $ show p

play :: [String] -> IO ()
play moves = case (execStateT (initialize moves >> solve) initialGrid) of
  Just result -> prettyPrint result
  Nothing -> putStrLn "Failed."
