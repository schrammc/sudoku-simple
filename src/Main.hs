module Main where

import Control.Monad
import Sudoku
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if ("-h" `elem` args) || ("--help" `elem` args) then printHelp else run

run :: IO ()
run = do
  done <- isEOF
  if done
    then return ()
    else do
      line <- getLine
      case parseSudoku line of
        Just s | (solution:_) <- solutions s -> putStrLn (formatBoardOneLine solution) >> run
        _ -> run

printHelp :: IO ()
printHelp = putStrLn "\
  \Sudoku solver\n\
  \\n\
  \Reads Sudokus from STDIN. Prints solved Sudokus. Example input:\n\
  \1...5.8.3..817..544.2..36.....534...36.7..............2....7......685.7.8...4...."
