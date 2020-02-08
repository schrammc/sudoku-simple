{-# LANGUAGE MultiWayIf #-}
module Sudoku where

import           Data.Char (isDigit)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Maybe

newtype Sudoku = Sudoku (IntMap Int)
                 deriving (Show, Eq)

exampleSudoku :: String
exampleSudoku = "769.....8....5....8.....34.....34..7.8...7..........9.63..78.....5.69......2.54.."

printBoard :: Sudoku -> IO ()
printBoard = putStrLn . formatBoard

formatBoardOneLine :: Sudoku -> String
formatBoardOneLine (Sudoku mp) = concat $ do
  i <- [0 .. 80]
  return $ fromMaybe "." $ fmap show $ IM.lookup i mp

formatBoard :: Sudoku -> String
formatBoard (Sudoku mp) = go [0 .. 80]
  where
    go [] = ""
    go (n:ns) =
      let val = fromMaybe "." $ fmap show $ IM.lookup n mp
          rest = val ++ go ns
      in if | n > 0 && n `mod` 27 == 0 -> "\n---+---+---\n" ++ rest
            | n > 0 && n `mod` 9 == 0 ->"\n" ++ rest
            | n > 0 && n `mod` 3 == 0 -> "|" ++ rest
            | otherwise -> rest

isSolved :: Sudoku -> Bool
isSolved = undefined

valuesInBox :: Sudoku -> Int -> [Int]
valuesInBox (Sudoku mp) pos = do
  let currentCol = pos `mod` 9
      currentRow = pos `div` 9
  row <- [(currentRow `div` 3) * 3 .. (currentRow `div` 3) * 3 + 2]
  col <- [(currentCol `div` 3) * 3 .. (currentCol `div` 3) * 3 + 2]
  let k = row * 9 + col
  maybe [] (:[]) (IM.lookup k mp)

valuesInRow :: Sudoku -> Int -> [Int]
valuesInRow (Sudoku mp) pos = do
  let rowStart = pos - (pos `mod` 9)
  k <- [rowStart .. rowStart + 9]
  maybe [] (:[]) (IM.lookup k mp)

valuesInColumn :: Sudoku -> Int -> [Int]
valuesInColumn (Sudoku mp) pos = do
  row <- [0 .. 8]
  let k = (row * 9) + (pos `mod` 9)
  maybe [] (:[]) (IM.lookup k mp)

parseSudoku :: String -> Maybe Sudoku
parseSudoku xs
  | length xs /= 81 && any (not . validChar) xs = Nothing
  | otherwise =
    Just
    $ Sudoku
    $ IM.fromList
    $ fmap (fmap readInt)
    $ filter ((/= '.') . snd)
    $ zip [(0 :: Int) ..] xs
  where
    validChar c = isDigit c || c == '.'
    readInt c = read [c]
