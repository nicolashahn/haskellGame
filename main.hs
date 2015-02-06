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

-- Print a square at given indices
makeSquare :: Int -> Int -> Color -> Picture
makeSquare x y col = translate (getC x) (getC y) 
                     $ color col 
                     $ rectangleSolid cellFloat cellFloat 

-- Create a list of square pictures
gridSquares :: [(Int, Int)] -> [Picture]
gridSquares indices = map (\(x, y) -> makeSquare x y $ col x y) indices
    where isEven n = n `mod` 2 == 0
          col x y = if (isEven x && isEven y) || ((not (isEven x)) && not (isEven y)) then white else (greyN (0.8))

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

data Cell = Cell Bacteria Position
    deriving (Eq, Show)

initialBoard :: Board
initialBoard = Play
            [ Cell 1 (0, 0) ] [ Cell 1 (0, 10) ]

------------------------------------------------------------------------------
-- Game state --
------------------------------------------------------------------------------
drawBoard :: Board -> Picture
drawBoard GameOver
    = scale 0.5 0.5
    $ translate (-400.0) (0.0)
    $ color red
    $ text  "GAME OVER"

drawBoard (Play cellsP cellsE) 
    = pictures [printGrid, colonyP, colonyE]
    where
    printGrid = pictures (gridSquares $ indices grid)
    colonyP = pictures [(makeSquare x y green) <> translate (getC x) (getC y) (color black (scale 0.2 0.2 $ text (show n)))
                       | Cell n (x, y) <- cellsP]
    colonyE = pictures [(makeSquare x y yellow) <> translate (getC x) (getC y) (color blue (scale 0.2 0.2 $ text (show n)))
                       | Cell n (x, y) <- cellsE]

------------------------------------------------------------------------------
-- Simulation --
------------------------------------------------------------------------------
simulateBoard :: Float -> (Board -> Board)

simulateBoard _ GameOver = GameOver

simulateBoard timeStep (Play cellsP cellsE)
    | length cellsP >= 25 = GameOver
    | otherwise = Play (Cell 1 (0, 0) : (concatMap updateCell cellsP)) (Cell 1 (0, 10) : concatMap updateCell cellsE)
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos) = [Cell (b + 1) ((fst pos + 1), snd pos)]

------------------------------------------------------------------------------
-- Event handling --
------------------------------------------------------------------------------
handleEvents :: Event -> Board -> Board

handleEvents _ GameOver = GameOver

-- EventKey     Key Keystate Modifiers (Float, Float)
handleEvents (EventKey (MouseButton LeftButton) Down _ _) 
             (Play cellsP cellsE)
    | length cellsP >= 20 = GameOver
    | otherwise = Play (Cell 1 (0, 0) : (concatMap updateCell cellsP)) (Cell 1 (0, 10) : concatMap updateCell cellsE)
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos) = [Cell (b + 1) ((fst pos - 1), snd pos)]

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
