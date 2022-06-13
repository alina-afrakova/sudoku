module Sudoku where

import Control.Monad
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Char
import Data.List

--------------
-- Data types.
--------------

-- representation of one sudoku cell
type Cell = Maybe Int
 
-- representation of sudoku table
data Sudoku = Sudoku [[Cell]]
  deriving (Show, Eq)

type Pos = (Int,Int)

type Cells = [(Pos, Cell)]

type CellsInt = [(Pos, Int)]

{-| A Block is a list of 9 Maybe Int(Cell) values. 
 Each Block represents a row, a column, or a square. |-}
--type Block = [Cell]


--------------------------------
-- Conversions helper functions.
--------------------------------

-- Gets Sudoku rows
rows :: Sudoku -> [[Cell]]
rows (Sudoku rs) = rs


-- Converts Sudoku into a list of cells and their coordinates
sudokuToCells :: Sudoku -> Cells
sudokuToCells sudoku = (zip (genCoords) (flatten (rows sudoku)))
  where
    genCoords = [(x, y) | y <- [8,7..0], x <- [0..8]]
    flatten = foldr1 (\x acc -> x ++ acc)


-- Converts Sudoku into BoolSudoku (Just a -> True, Nothing -> False)
--boolSudoku :: [[Cell]] -> BoolSudoku
--boolSudoku (row : rows) = (map (\c -> (c/=Nothing)) row) : (boolSudoku rows)


intToCellsInt :: [[Int]] -> CellsInt
intToCellsInt x = (zip (genCoords) (flatten x))
  where
    genCoords = [(x, y) | y <- [8,7..0], x <- [0..8]]
    flatten = foldr1 (\x acc -> x ++ acc)

cellsIntToInt :: CellsInt -> [Int]
cellsIntToInt = map snd


cellsToInt :: [[Cell]] -> [[Int]]
cellsToInt = map $ map oneCellToInt
  where
    oneCellToInt :: Cell -> Int
    oneCellToInt (Just x) = x
    oneCellToInt (Nothing) = 0


cellsIntToSquare :: CellsInt -> [[Int]]
cellsIntToSquare x = genCoords x
  where
    genCoords z = [ (squareCells z x y) | y <- [0,3,6], x <- [0,3,6]]
    squareCells s a b = cellsIntToInt (filter (\((x,y), z)->(x>=a && y>=b && x<a+3 && y<b+3)) s)


------------------------
-- Game Logic functions.
------------------------

-- Checks if the cell is not filled
notFilled :: Sudoku -> Pos -> Bool
notFilled sud pos = getCell (sudokuToCells sud) pos == Nothing
  where
    getCell (c : cs) pos | fst c == pos = snd c
                         | otherwise = getCell cs pos 


-- Сhanges the cell to a new one at the specified coordinates
changeCell :: [[Cell]] -> Pos -> Int -> Cell -> [[Cell]]
changeCell (row : rows) (x, y) i new_cell | i==y = (changeRow row x 0 new_cell) : rows
                                          | otherwise = row : (changeCell rows (x, y) (i-1) new_cell)
  where 
    changeRow (c : cs) x j new_cell | j==x = (new_cell : cs)
                                    | j/=x = c : (changeRow cs x (j+1) new_cell)



-- Checks if the player won
winPlayer :: Sudoku -> Bool
winPlayer s | sum (map noNothing (rows s)) /= 0 = False
            | otherwise = sudokuCheck (cellsToInt $ rows s)


noNothing :: [Cell] -> Int
noNothing [] = 0
noNothing (x:xs) | x==Nothing = 1
                 | otherwise = noNothing xs

sudokuCheck :: [[Int]] -> Bool
sudokuCheck s | rowsCheck (map sort s) == False = False
              | rowsCheck (map sort (transpose s)) == False = False
              | rowsCheck (map sort (cellsIntToSquare (intToCellsInt s))) == False = False
              | otherwise = True

rowsCheck :: [[Int]] -> Bool
rowsCheck [] = True
rowsCheck (x:xs) = (rowCheck x) && (rowsCheck xs)
  where
    rowCheck z = snd (foldr (\x (y, acc) -> (x, ((x/=y) && acc))) (0, True) z)



-- Gives the list of numbers that can be placed in the cell (hints)
possibleDigits :: Sudoku -> Pos -> [Int]
possibleDigits sud pos = [1..9] \\ (nub $ (getBlocksDigits (cellsToInt $ rows sud) pos))


getBlocksDigits :: [[Int]] -> Pos -> [Int]
getBlocksDigits sud (x, y) = (getRow sud y) ++ (getCol sud x) ++ (getSquare sud (x, y))

getRow :: [[Int]] -> Int -> [Int]
getRow rows y = (reverse rows) !! y

getCol :: [[Int]] -> Int -> [Int]
getCol rows x = (transpose $ reverse rows) !! x

getSquare :: [[Int]] -> Pos -> [Int]
getSquare rows (x, y) = (cellsIntToSquare (intToCellsInt rows)) !! ((y `div` 3) * 3 + (x `div` 3))


-- Checks if the cell conflicts with others
hasConflicts :: Sudoku -> (Pos, Cell) -> Bool
hasConflicts sud (pos, Just x) = (digitRepeat x $ getDigits sud pos) > 3
  where
    getDigits sud pos = getBlocksDigits (cellsToInt $ rows sud) pos
    digitRepeat x = foldr (\y acc -> if y==x then acc+1 else acc) 0


{-
cellsToSudoku :: Cells -> Sudoku
cellsToSudoku c = Sudoku (cellsToRows (map (\x -> snd x) c))

cellsToRows :: [Cell] -> [[Cell]]
cellsToRows [] = []
cellsToRows c = (take 9 c) : (cellsToRows (drop 9 c)) 


changeCell2 :: Cells -> Pos -> Cell -> Cells
changeCell2 [] _ _ = []
changeCell2 (((x,y), old_cell) : cs)  (x',y')  new_cell 
  | (x==x' && y==y' && (old_cell==Nothing || new_cell==Nothing)) = ((x,y), new_cell) : cs
  | otherwise = ((x,y), old_cell) : (changeCell2 cs (x',y') new_cell)


-- Checks if it's really a sudoku
isSudoku :: Sudoku -> Bool
isSudoku x = (length (rows x) == 9) && ( and [length y == 9 | y <- rows x]) && ( and [x > 0 && x < 10 | Just x <- concat (rows x)])


isValidBlock :: Block -> Bool
isValidBlock xs = (\x -> x == nub x) (filter (/=Nothing) xs)
-}


---------------------------------------------
-- Dirty IO stuff - reading sudoku from file.
---------------------------------------------

loadSudoku :: String -> IO Sudoku
loadSudoku filename = do
    content <- readFile filename
    let 
      rows = lines content
      sudoku = parseSudoku rows
    return sudoku


parseSudoku :: [String] -> Sudoku
parseSudoku lines = Sudoku ( 
  [ [if char == '0' || char == '.' then Nothing else Just (digitToInt char) | char <- stripPipes line] 
                                                                            | line <- stripVoids lines] )
  where 
    stripVoids = filter (/= "")
    stripPipes = filter (/= '|')

