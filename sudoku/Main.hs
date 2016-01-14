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
doMove :: Move -> SudokuState ()
doMove ((x,y),value) = update x y value
doMoves :: [Move] -> SudokuState ()
doMoves = mapM_ doMove
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

main :: IO ()
main = do
    print "Hi"
