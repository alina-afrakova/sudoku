module Picture where

import Sudoku

import Graphics.Gloss.Interface.Pure.Game


------------
-- Constants.
-------------

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = white

-- Simulation steps per second.
fps :: Int
fps = 60

-- Text shift on screen.
textShift :: Float
textShift = 100

cellLength :: Float
cellLength = 40

cellColor :: Color
cellColor = (greyN 0.92)
--cellColor = (mixColors 0.8 0.2 white black)

textInfo :: [String]
textInfo = ["COMMANDS:", "Arrow Key -- Move focus", "1 - 9 -- Input number", "Delete/Backspace -- Erase number", "Space -- Check for winning", "h -- Show/Hide hints", "c -- Clear all and reset timer", "Esc -- Quit"]


----------------------------
-- Pure rendering functions.
----------------------------

renderSudoku :: Sudoku -> [Pos] -> Sudoku -> Picture
renderSudoku sud mistakes initial = Pictures [ 
    Translate (((fromIntegral x) - 4) * cellLength) (((fromIntegral y) - 4) * cellLength) $ 
    ( renderCell cell isMistaken isInitial ) | ((x, y), cell) <- (sudokuToCells sud),
    let isMistaken = (x, y) `elem` mistakes,
    let isInitial = ((reverse $ rows initial) !! y !! x) /= Nothing
    --let initialPos = map (\x -> fst x) $ filter (\(pos, cell) -> cell /= Nothing) $ sudokuToCells initial,
    --let isInitial = (x, y) `elem` initialPos
  ]

renderCell :: Cell -> Bool -> Bool -> Picture
renderCell cell mistake initial = Pictures [
    color cellColor $ rectangleSolid cellLength cellLength,
    color (greyN 0.7) $ rectangleWire cellLength cellLength,
    color colour $ Translate (-1/4*cellLength) (-1/4*cellLength) $ scale 0.2 0.2 $ text $ maybe "" show cell
  ]
  where
    colour = if mistake then red 
             else if initial then black
             else blue

renderBlocks :: Picture
renderBlocks = Pictures [(renderBlock (x,y)) | x <- [1,4,7], y <- [1,4,7]]
  where
    renderBlock (x, y) = Pictures [
        color black $ Translate (((fromIntegral x) - 4) * cellLength) (((fromIntegral y) - 4) * cellLength) $ 
        rectangleWire (cellLength*3) (cellLength*3)
       ]

renderCoords :: Pos -> Picture
renderCoords (x, y) = Pictures [
    Translate (((fromIntegral x) - 4) * cellLength) (((fromIntegral y) - 4) * cellLength) $ Color red $
    rectangleWire cellLength cellLength
  ]

renderEnd :: Picture
renderEnd = Pictures [
    color red $ Translate 0 (7*cellLength) $ rectangleSolid (cellLength*8) (cellLength*2),
    color black $ Translate (-2*cellLength - 3/4*cellLength) (7*cellLength - 1/2*cellLength) $ scale 0.4 0.4 $ text win ]
    where
          win = "You win!"


renderHints :: [Int] -> Picture
renderHints hints = Pictures [
    color (dark blue) $ Translate (-4 * cellLength) (-6 * cellLength) $ scale 0.2 0.2 $ text $ "Hints (possible digits): " ++ showHints hints
  ]
  where
    showHints [] = ""
    showHints [x] = show x
    showHints (x : xs) = show x ++ "," ++ showHints xs

renderTime :: Float -> Picture
renderTime time = Pictures [
    color black $ Translate (6 * cellLength) 0 $ scale 0.4 0.4 $ text $ "Timer: " ++ showTime min ++ ":" ++ showTime sec
  ]
  where 
    min = (round time) `div` 60
    sec = (round time) `mod` 60
    showTime t = if (t `div` 10 == 0) then (show 0 ++ show t) else show t

renderInfo :: Picture
renderInfo = Pictures [
    color black $ Translate (-15 * cellLength) ((4 - i) * cellLength) $ scale 0.15 0.15 $ text info | (i, info) <- enumerate 0 textInfo
  ]
  where
    enumerate _ [] = []
    enumerate i (x : xs) = (i, x) : (enumerate (i+1) xs) 
