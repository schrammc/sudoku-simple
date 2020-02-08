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
