-- main.hs

import Data.Monoid ((<>))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Data.List
import System.Random

------------------------------------------------------------------------------
-- Initialization --
------------------------------------------------------------------------------

gridLength :: Int
gridLength = 25 -- length of grid

cellSize :: Int
cellSize = 25 -- cell's pixel height/width
cellFloat :: Float
cellFloat = fromIntegral cellSize

winSize :: Int
winSize = gridLength * cellSize -- the windows' height/width in pixels
winFloat :: Float
winFloat = fromIntegral winSize

grid = rectSquareGrid gridLength gridLength

data Board = Play Player Enemy StdGen 
          | GameOver
          deriving (Show)

type Bacteria = Int
type Position = (Int, Int)
type Player = [Cell]
type Enemy = [Cell]

data Cell = Cell Bacteria Position Color
    deriving (Eq, Show)

initialBoard :: StdGen -> Board 
initialBoard gen = Play
            [Cell 1 (gridLength `div` 4, gridLength `div` 2) green] 
            [Cell 1 (gridLength - (gridLength `div` 4), gridLength `div` 2) yellow]
            gen

------------------------------------------------------------------------------
-- Game state --
------------------------------------------------------------------------------

-- helper fn
drawColony cells = pictures [makeSquare x y col <> showNum n x y
                            | Cell n (x, y) col <- cells]

drawBoard :: Board -> Picture
drawBoard GameOver
    = scale 0.5 0.5
    $ translate (-400.0) (0.0)
    $ color red
    $ text  "GAME OVER"

drawBoard (Play cellsP cellsE gen) 
    = pictures [printGrid, drawColony cellsP, drawColony cellsE]
        where
        printGrid = pictures (gridSquares $ indices grid)

------------------------------------------------------------------------------
-- Simulation --
------------------------------------------------------------------------------

-- probability that bacteria will grow
-- probGrowth = 1

-- takes list of cell positions
-- returns list of adjacent positions
adjCells :: [Position] -> [(Position,Position)]
adjCells [] = []
adjCells ps = (foldl (\a p -> a ++ (map (\x -> (x,p)) (neighbours grid p))) [] ps)

-- returns positions of empty cells around border of clony
--borderCells :: [Position] -> [Position] -> [Position]
--borderCells [] __ = []
--borderCells ps filledCells = adjCells ps \\ filledCells


-- 
dropFst :: [(Position,Position)] -> [Position] -> [(Position,Position)]
dropFst [] _ = []
dropFst _ [] = []
dropFst (c:cs) fs = if (elem (snd c) fs) then c:(dropFst cs fs)
										else (dropFst cs fs)

-- takes colony c1 and c2
-- finds all the adjacent positions to c1
-- remove from the list all the positions that are already taken by c1 or c2
-- return a list of both the empty border positions
-- and the position of the cell it was adjacent to (the cell that can spawn a new cell)
borderPos :: [Cell] -> [Cell] -> [(Position,Position)]
borderPos c1 c2 = dropFst (adjCells (colonyPos c1)) filledCells
							where filledCells = (colonyPos c1 ++ colonyPos c2)

-- combines new cells
combine :: [Position] -> Color -> [Cell]
combine [] _ = []
combine borderPos col = (Cell 1 (head borderPos) col) : combine (tail borderPos) col

-- increases size of colony
growColony :: [Cell] -> [Cell] -> [Cell]
growColony c1 c2 = combine (spawnable) (cellColor $ head c1) ++ c1
    --where borderPos c1 c2 = borderCells (colonyPos c1) (colonyPos c1 ++ colonyPos c2)
    where spawnable = map fst (borderPos c1 c2)
-- updates population of one cell 
upCellPop :: Cell -> Cell
upCellPop c@(Cell pop xy col) = if pop < 10
                              -- replace this 1 with a randZeroOne when it works
                              then (Cell (pop + (1)) xy col)
                              else c

-- update list of bacteria's population
updateCells :: [Cell] -> [Cell]
updateCells [] = []
updateCells cells = map upCellPop cells




-- take a previous game state and return the new game state after given time
simulateBoard :: Float -> (Board -> Board)
simulateBoard _ GameOver = GameOver
simulateBoard timeStep (Play cellsP cellsE gen)
    | length cellsP >= 200 = GameOver
    | otherwise = Play (fullUpdate cellsP cellsE) (fullUpdate cellsE cellsP) newGen
    where
        (probGrowth, newGen) = randomR(1, 2) gen :: (Int, StdGen)
        fullUpdate c1 c2 
            | probGrowth == 1 = growColony (updateCells c1) c2
            | otherwise = updateCells c1
    -- = let   (probGrowth, newGen) = randomR(1, 2) gen :: (Int, StdGen)
    --   in    Play (fullUpdate cellsP cellsE) (fullUpdate cellsE cellsP) newGen
    --     where
    --         fullUpdate c1 c2 
    --             | probGrowth == 1 = growColony (updateCells c1) c2
    --             | otherwise = updateCells c1
    --


------------------------------------------------------------------------------
-- Event handling --
------------------------------------------------------------------------------
handleEvents :: Event -> Board -> Board
handleEvents _ GameOver = GameOver
handleEvents (EventKey (MouseButton LeftButton) Down _ _)
             (Play cellsP cellsE gen)
    | length cellsP >= 20 = GameOver
    | otherwise = Play (Cell 1 (0, 0) blue : (concatMap updateCell cellsP)) (Cell 1 (0, 10) yellow : concatMap updateCell cellsE) gen
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos col) = [Cell (b + 1) ((fst pos + 1), snd pos) col]

handleEvents _ board = board  -- all other possible events

------------------------------------------------------------------------------
-- Helper functions --
------------------------------------------------------------------------------
-- Make coordinates correct bc gloss sets origin at center
getC :: Int -> Float
getC x = ((cellFloat) * (fromIntegral x)) - ((winFloat) / 2) + ((cellFloat) / 2)

-- Create picture of a colored square at given indices
makeSquare :: Int -> Int -> Color -> Picture
makeSquare x y col 
    = translate (getC x) (getC y) 
    $ color col 
    $ rectangleSolid cellFloat cellFloat 

-- Create a list of pictures of colored squares
gridSquares :: [(Int, Int)] -> [Picture]
gridSquares indices = map (\(x, y) -> makeSquare x y $ col x y) indices
    where isEven n = n `mod` 2 == 0
          col x y = if (isEven x && isEven y) || ((not (isEven x)) && not (isEven y)) then white else (greyN (0.8))

-- Create a picture of a number at given coordinates
showNum :: Int -> Int -> Int -> Picture
showNum i x y
    =  translate (getC(x) - cellFloat/5) (getC(y)-cellFloat/4)
    $ scale (0.12) (0.12)
    $ color black
    $ text (show i)

-- Returns list of positions for colony
colonyPos :: [Cell] -> [Position]
colonyPos cells = map cellPos cells

-- get cell's color
cellColor :: Cell -> Color
cellColor (Cell _ _ col) = col

 -- get cell's position
cellPos :: Cell -> (Int, Int)
cellPos (Cell _ xy  _) = xy

-- get cell's population
cellPop :: Cell -> Int
cellPop (Cell p _ _) = p

------------------------------------------------------------------------------
-- Main --
------------------------------------------------------------------------------
main
 = do   gen <- getStdGen
        play (InWindow "Grid" (winSize, winSize) (0, 0)) 	 -- window positioned in center
             white
             1
             (initialBoard gen)
             drawBoard
             handleEvents
             simulateBoard
