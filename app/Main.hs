module Main where

import Control.Monad (forM_, forM)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import Data.Int (Int8)
import System.Clock (getTime, Clock(Monotonic), toNanoSecs, diffTimeSpec)

type Row = IOVector Int8
type Sudoku = IOVector Row

-- Initialize a Sudoku grid from a list
initializeSudoku :: [Int8] -> IO Sudoku
initializeSudoku elems = do
    sudoku <- MV.new 9
    forM_ [0..8] $ \i -> do
        row <- MV.new 9
        forM_ [0..8] $ \j -> MV.write row j (elems !! (i * 9 + j))
        MV.write sudoku i row
    return sudoku


-- Print the Sudoku grid
showSudoku :: Sudoku -> IO ()
showSudoku sudoku = do
    forM_ [0..8] $ \i -> do
        row <- MV.read sudoku i
        rowElems <- forM [0..8] $ \j -> MV.read row j
        putStrLn $ unwords $ map showElem rowElems
    where
        showElem 0 = "."
        showElem n = show n


-- Find the first empty cell in the Sudoku grid
findEmpty :: Sudoku -> IO (Int, Int)
findEmpty sudoku = go 0 0
  where
    go 9 _ = return (-1, -1)  -- All rows checked, no empty cell found
    go i 9 = go (i + 1) 0   -- End of row, move to the next row
    go i j = do
      row <- MV.read sudoku i
      value <- MV.read row j
      if value == 0
        then return (i, j)  -- Found an empty cell, return 1-based index
        else go i (j + 1)  -- Check next cell in the row


-- Check if placing a number at a given position is valid
valid :: Sudoku -> Int8 -> (Int, Int) -> IO Bool
valid sudoku num (row, col) = do
    -- Check row
    rowValid <- checkRow sudoku row num col
    if not rowValid then return False else do
      -- Check column
      colValid <- checkColumn sudoku col num row
      if not colValid then return False else do
        -- Check box
        let boxStartRow = (row `div` 3) * 3
        let boxStartCol = (col `div` 3) * 3
        boxValid <- checkBox sudoku boxStartRow boxStartCol num (row, col)
        return boxValid

-- Check the row for the same number, ignoring the column position
checkRow :: Sudoku -> Int -> Int8 -> Int -> IO Bool
checkRow sudoku rowIndex num colIndex = do
    row <- MV.read sudoku rowIndex
    allDifferent row num colIndex

-- Check each element in the row vector
allDifferent :: IOVector Int8 -> Int8 -> Int -> IO Bool
allDifferent row num colIndex = go 0
  where
    go i
      | i == MV.length row = return True
      | otherwise = do
          x <- MV.read row i
          if x == num && i /= colIndex then return False
          else go (i + 1)

-- Check the column for the same number, ignoring the row position
checkColumn :: Sudoku -> Int -> Int8 -> Int -> IO Bool
checkColumn sudoku colIndex num rowIndex = go 0
  where
    go i
      | i == 9 = return True
      | otherwise = do
          row <- MV.read sudoku i
          x <- MV.read row colIndex
          if x == num && i /= rowIndex then return False
          else go (i + 1)

-- Check the 3x3 box
checkBox :: Sudoku -> Int -> Int -> Int8 -> (Int, Int) -> IO Bool
checkBox sudoku boxStartRow boxStartCol num pos = go boxStartRow
  where
    go i
      | i == boxStartRow + 3 = return True
      | otherwise = do
          row <- MV.read sudoku i
          rowValid <- goRow row (boxStartCol) (boxStartCol + 3) i
          if not rowValid then return False
          else go (i + 1)
    goRow row startCol endCol i = goCol startCol
      where
        goCol j
          | j == endCol = return True
          | otherwise = do
              x <- MV.read row j
              if x == num && (i, j) /= pos then return False
              else goCol (j + 1)


-- Solve the Sudoku using a backtracking algorithm
solve :: Sudoku -> IO Bool
solve sudoku = do
    emptyPos <- findEmpty sudoku
    case emptyPos of
      (-1, -1) -> return True  -- No empty spots, solution found
      (row, col) -> tryFilling row col [1..9]  -- Try numbers 1 through 9
  where
    tryFilling :: Int -> Int -> [Int8] -> IO Bool
    tryFilling _ _ [] = return False  -- No numbers left to try, backtrack
    tryFilling row col (n:ns) = do
      validMove <- valid sudoku n (row, col)
      if validMove
        then do
          rowVec <- MV.read sudoku row
          MV.write rowVec col n  -- Place the number
          solved <- solve sudoku  -- Recurse to solve the rest
          if solved
            then return True
            else do
              MV.write rowVec col 0  -- Backtrack: reset the cell
              tryFilling row col ns  -- Continue with the next number
        else tryFilling row col ns  -- Number not valid, try the next one


main :: IO ()
main = do
  let mat1 = [5,3,0,0,7,0,0,0,0,
              6,0,0,1,9,5,0,0,0,
              0,9,8,0,0,0,0,6,0,
              8,0,0,0,6,0,0,0,3,
              4,0,0,8,0,3,0,0,1,
              7,0,0,0,2,0,0,0,6,
              0,6,0,0,0,0,2,8,0,
              0,0,0,4,1,9,0,0,5,
              0,0,0,0,8,0,0,7,9]

  let mat2 = [9,0,3,0,0,0,0,5,0,
            0,0,7,0,0,0,0,8,0,
            0,0,0,0,1,0,0,0,0,
            0,0,0,0,0,7,0,3,0,
            4,1,0,0,0,0,2,0,0,
            0,0,0,0,0,5,0,0,0,
            0,2,0,0,4,0,1,0,0,
            0,0,0,9,0,0,0,0,0,
            0,0,5,0,0,0,0,0,0]

  let mat3 = [0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,3,0,8,5,
            0,0,1,0,2,0,0,0,0,
            0,0,0,5,0,7,0,0,0,
            0,0,4,0,0,0,1,0,0,
            0,9,0,0,0,0,0,0,0,
            5,0,0,0,0,0,0,7,3,
            0,0,2,0,1,0,0,0,0,
            0,0,0,0,4,0,0,0,9]

  sudoku <- initializeSudoku mat2
  showSudoku sudoku

  start <- getTime Monotonic
  solved <- solve sudoku
  end <- getTime Monotonic

  let diff = fromIntegral (toNanoSecs (diffTimeSpec end start)) / (1000000) :: Double
  putStrLn $ "Solving took " ++ show diff ++ " milliseconds."

  print solved
  showSudoku sudoku
