module Sudoku (SudokuBoard(..), isValidSolution) where

import Data.List (transpose)
import Data.List.Split (chunksOf)

newtype SudokuBoard = SudokuBoard {
    get :: [[Int]]
} deriving (Show, Eq)

isValidSolution :: SudokuBoard -> Bool
isValidSolution board =
  let rowsAreValid     = all axisIsValid $ get board
      colsAreValid     = all axisIsValid $ transpose (get board)
      subGridsAreValid = all axisIsValid $ constructSubGrids (get board)
   in rowsAreValid && colsAreValid && subGridsAreValid

axisIsValid :: [Int] -> Bool
axisIsValid xs = length xs == 9 && unique xs

constructSubGrids :: [[Int]] -> [[Int]]
constructSubGrids = concatMap (concatMap chunk . chunk)
  where chunk :: [Int] -> [[Int]]
        chunk = (take 3 . drop 3) . chunksOf 3

unique :: (Eq a) => [a] -> Bool
unique []     = True
unique (x:xs) = x `notElem` xs && unique xs
