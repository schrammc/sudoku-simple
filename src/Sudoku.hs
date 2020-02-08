module Sudoku where

import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

newtype Sudoku = Sudoku (IntMap Int)
                 deriving (Show, Eq)

exampleSudoku :: String
exampleSudoku = "769.....8....5....8.....34.....34..7.8...7..........9.63..78.....5.69......2.54.."

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
