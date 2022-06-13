module State where

import Sudoku

import Data.Char (digitToInt)
import System.Exit (exitSuccess)
import Graphics.Gloss.Interface.Pure.Game

--------------
-- Data types.
--------------
 
-- General application state.
data AppState = AppState 
  { sudoku :: Sudoku -- Sudoku board itself
  , coords :: Pos -- Current cell coordinates
  , initial :: Sudoku -- Initial filled board 
  , win :: Bool -- True - end the game
  , hints :: (Bool, [Int]) -- Hints for the cell (shows which numbers can be placed in the cell)
  , mistakes :: [Pos] -- Posiitions of incorrectly filled cells
  , time :: Float -- Timer
  --, history :: History -- History stores the previous game sessions results
  --, filename :: Maybe FilePath
  }


------------------
-- Pure functions.
------------------

-- Handle events
handleEvent :: Event -> AppState -> AppState

-- Changing current coords
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state = 
  updateAppState state (moveCoords (coords state) 0 1) (sudoku state)
  --state {coords = moveCoords (coords state) 0 1}
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state = 
  updateAppState state (moveCoords (coords state) 0 (-1)) (sudoku state)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state = 
  updateAppState state (moveCoords (coords state) (-1) 0) (sudoku state)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state = 
  updateAppState state (moveCoords (coords state) 1 0) (sudoku state)

-- Handle other keys
handleEvent (EventKey (SpecialKey k) Up _ _) state 
  | k == KeyDelete || k == KeyBackspace = -- Erase
    updateAppState state (coords state) $ makeMove (sudoku state) (initial state) (coords state) Nothing
  | k == KeySpace = -- Win or not
    state { win = winPlayer (sudoku state) }
  | otherwise = state

handleEvent (EventKey (Char c) Up _ _) state 
  | '1' <= c && c <= '9' = -- Input
    updateAppState state (coords state) $ makeMove (sudoku state) (initial state) (coords state) (Just $ digitToInt c)
  | c == '\b' = -- Erase
    updateAppState state (coords state) $ makeMove (sudoku state) (initial state) (coords state) Nothing
  | c == 'h' && fst (hints state) == False = -- Show hint
    state { hints = getHints (sudoku state) (initial state) (coords state), 
            mistakes = checkMistakes (sudoku state) }
  | c == 'h' && fst (hints state) == True = -- Hide hint
    state { hints = (False, []), mistakes = [] }
  | c == 'c' = -- Clear
    state { sudoku = (initial state), time = 0 }
  | otherwise = state

-- Ignore all other events
handleEvent _ state = state


-- Update state, save sudoku after making moves
updateAppState :: AppState -> Pos -> Sudoku -> AppState
updateAppState state coord sud | fst (hints state)==False && (win state)==False
                                  = state { sudoku=sud, coords=coord }
                               | otherwise = state -- don't change the state


-- Update current cell coordinates
moveCoords :: Pos -> Int -> Int -> Pos
moveCoords (x, y) dx dy =
  let 
    x' = x + dx
    y' = y + dy
  in
    if 0 <= x' && x' <= 8 && 0 <= y' && y' <= 8
    then (x', y') else (x, y)


-- Make moves with checking of initially filled cells
makeMove :: Sudoku -> Sudoku -> Pos -> Cell -> Sudoku
makeMove sudoku init coords cell | (notFilled init coords) = Sudoku (changeCell (rows sudoku) coords 8 cell)
                                 | otherwise = sudoku

getHints :: Sudoku -> Sudoku -> Pos -> (Bool, [Int])
getHints sudoku init coords | (notFilled sudoku coords) = (True, possibleDigits sudoku coords)
                            | otherwise = (True, [])

checkMistakes :: Sudoku -> [Pos]
checkMistakes sudoku = [pos | (pos, cell) <- sudokuToCells sudoku, cell /= Nothing && hasConflicts sudoku (pos, cell)]
