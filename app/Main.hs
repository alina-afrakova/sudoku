module Main where

import Sudoku
import State
import Picture

import System.Environment (getArgs)
import Graphics.Gloss.Interface.Pure.Game


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "TOO FEW ARGUMENTS"
    [file] -> do
        sudoku <- (loadSudoku file)
        --print sudoku
        let initState = AppState{ 
           sudoku = sudoku
         , coords = (0,8)
         , initial = sudoku
         , win = False 
         , hints = (False, [])
         , mistakes = []
         , time = 0
         }
        play display bgColor fps initState renderApp handleEvent updateApp
    _  -> error "INVALID AMOUNT OF ARGUMENTS"


-- Simulation step (updates time)
updateApp :: Float -> AppState -> AppState
updateApp dt state | not (win state) = state { time = (time state) + dt }
                   | otherwise = state

-- Rendering all app
renderApp :: AppState -> Picture
renderApp (AppState sudoku coords initial win hints mistakes time) 
  | win==False && fst hints==False = Pictures 
   [ renderSudoku sudoku mistakes initial
   , renderBlocks
   , renderCoords coords
   , renderTime time
   , renderInfo
   ] 
  | fst hints==True = Pictures 
   [ renderSudoku sudoku mistakes initial
   , renderBlocks
   , renderCoords coords
   , renderHints $ snd hints
   , renderTime time
   , renderInfo
   ]
  | win==True = Pictures 
   [ renderSudoku sudoku mistakes initial
   , renderBlocks
   , renderEnd
   , renderTime time
   ]
