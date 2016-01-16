module Test where

import Sudoku

sample = ["A1:1", "C2:2", "D2:7", "E2:4", "D3:5", "I3:4", "B4:3", "A5:7", "B5:5", "F6:9", "G6:6", "B7:4", "F7:6", "H8:7", "I8:1", "F9:1", "H9:3"]

main :: IO ()
main = do
  play sample
