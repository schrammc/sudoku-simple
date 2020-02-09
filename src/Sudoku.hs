{-# LANGUAGE MultiWayIf #-}
module Sudoku where

import           Data.Char (isDigit)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe

newtype Sudoku = Sudoku (IntMap Int)
                 deriving (Show, Eq)

exampleSudoku :: String
exampleSudoku = "1...5.8.3..817..544.2..36.....534...36.7..............2....7......685.7.8...4...."

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

-- | Points on the sudoku field that don't have a number yet
freePoints :: Sudoku -> [Int]
freePoints (Sudoku mp) = filter (\x -> not (IM.member x mp)) [0 ..80]

solutions :: Sudoku -> [Sudoku]
solutions s = solvePoints s (freePoints s)

solvePoints :: Sudoku -> [Int] -> [Sudoku]
solvePoints sudoku [] = [sudoku]
solvePoints sudoku (p:ps) = do
  sudoku' <- pointSolutions sudoku p
  solvePoints sudoku' ps

pointSolutions :: Sudoku -> Int -> [Sudoku]
pointSolutions sudoku@(Sudoku mp) field =
  fmap (\value -> Sudoku (IM.insert field value mp)) (possibleValues sudoku field)

possibleValues :: Sudoku -> Int -> [Int]
possibleValues sudoku@(Sudoku mp) field
  | Just k <- IM.lookup field mp = []
  | otherwise =
    IS.toList
    $ IS.difference (IS.fromList [1..9])
    $ IS.fromList
    $ concat [ valuesInRow sudoku field
             , valuesInColumn sudoku field
             , valuesInBox sudoku field]

-- Check if a sudoku puzzle is solved
isSolved :: Sudoku -> Bool
isSolved s = rowsOK && columnsOK && boxesOK
  where
    rowsOK = all (== allNumbers) $ (IS.fromList  . valuesInRow s) <$> [0..8]
    columnsOK = all (== allNumbers) $ (IS.fromList  . valuesInColumn s) <$> [0..8]
    boxesOK = all (== allNumbers) $ (IS.fromList . valuesInBox s) <$> boxTopLeftPoints
    boxTopLeftPoints = concat $ take 3 $ iterate (fmap (+27)) [0, 3, 6]
    allNumbers = IS.fromList [1..9]

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
  k <- [rowStart .. rowStart + 8]
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
