-- main.hs

import Data.Monoid ((<>))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

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

data Board = Play Player Enemy 
          | GameOver
          deriving (Eq, Show)

type Bacteria = Int
type Position = (Int, Int)
type Player = [Cell]
type Enemy = [Cell]

data Cell = Cell Bacteria Position Color
    deriving (Eq, Show)

initialBoard :: Board
initialBoard = Play
            [Cell 1 (gridLength `div` 4, gridLength `div` 2) green] 
            [Cell 1 (gridLength - (gridLength `div` 4), gridLength `div` 2) yellow]

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

drawBoard (Play cellsP cellsE) 
    = pictures [printGrid, drawColony cellsP, drawColony cellsE]
        where
        printGrid = pictures (gridSquares $ indices grid)

------------------------------------------------------------------------------
-- Simulation --
------------------------------------------------------------------------------

-- probability that bacteria will grow
probGrowth = 0.5

-- updates population of one cell 
upCellPop :: Cell -> [Cell]
upCellPop c@(Cell pop xy col) = if pop < 3
                              -- replace this 1 with a randZeroOne when it works
                              then [(Cell (pop + (1)) xy col)]
                              else Cell 1 (fst xy, (snd xy + 1)) col : [c]

-- -- update list of bacteria's population
--  updateCells :: [Cell] -> [Cell]
--  updateCells [] = []
--  updateCells xs = map upCellPop xs

-- take a previous game state and return the new game state after given time
simulateBoard :: Float -> (Board -> Board)
simulateBoard _ GameOver = GameOver
simulateBoard timeStep (Play cellsP cellsE)
    | length cellsP >= 20 = GameOver
    -- | otherwise = Play (concatMap upCellPop cellsP) (Cell 1 (0, 10) yellow : concatMap upCellPop cellsE)
    | otherwise = Play (concatMap upCellPop cellsP) (Cell 1 (0, 10) yellow : concatMap upCellPop cellsE)

------------------------------------------------------------------------------
-- Event handling --
------------------------------------------------------------------------------
handleEvents :: Event -> Board -> Board
handleEvents _ GameOver = GameOver
handleEvents (EventKey (MouseButton LeftButton) Down _ _) 
             (Play cellsP cellsE)
    | length cellsP >= 20 = GameOver
    | otherwise = Play (Cell 1 (0, 0) blue : (concatMap updateCell cellsP)) (Cell 1 (0, 10) yellow : concatMap updateCell cellsE)
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos col) = [Cell (b + 1) ((fst pos + 1), snd pos) col]

handleEvents _ board = board  -- all other possible events

------------------------------------------------------------------------------
-- Main --
------------------------------------------------------------------------------
main 	
 = play 
        (InWindow
	     "Grid" 	 -- window title
		(winSize, winSize) 	 -- window size
		(0, 0)) 	 -- window positioned in center
	white			 -- background color
    1
    initialBoard
    drawBoard
    handleEvents
    simulateBoard
