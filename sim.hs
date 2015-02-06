-- sim.hs
-- sim test

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

-- state of the game
data Board = Play [Cell] 
          | GameOver
          deriving (Eq, Show)

type Population = Int
type Position = (Int, Int)
type Colony = Color

data Cell = Cell Population Position Color
    deriving (Eq, Show)

initialBoard :: Board
initialBoard = Play
            [ Cell 1 (0,0) red ] -- Default Population

drawBoard :: Board -> Picture
drawBoard GameOver
    = scale 0.5 0.5
    $ translate (-400.0) (0.0)
    $ color red
    $ text  "GAME OVER"

drawBoard (Play cells) 
    = pictures [printPic, colony]
    where
    colony = pictures [translate (getC x) (getC y) (color col (scale 0.2 0.2 $ text (show n)))
                      | Cell n (x, y) col <- cells]

-----------------------------------------------------------
-- print grid
-----------------------------------------------------------
gLength :: Int
gLength = 25 -- length of grid (square grid's height or width in cells)
cellSize :: Int
cellSize = 25 -- cell's pixel height/width
cellFloat :: Float
cellFloat = fromIntegral cellSize
winSize :: Int
winSize = gLength * cellSize -- the windows' height/width in pixels
winFloat :: Float
winFloat = fromIntegral winSize
gSize = gLength * gLength -- total # of cells
grid = rectSquareGrid (fromIntegral gLength) (fromIntegral gLength)
gCoors = indices grid --list of grid coordinates

-- Make coordinates correct bc gloss sets origin at center
getC :: Int -> Float
getC x = ((cellFloat) * (fromIntegral x)) - ((winFloat) / 2) + ((cellFloat) / 2)

-- converts Grid coordinates to pixel coordinates for printing purposes
coorsToPixels coors = map (\(x, y) -> (getC x, getC y)) coors
    --where convert a = fromIntegral(a * gLength)

-- list of pixel coordinates
gPixelCoors = coorsToPixels (gCoors)

-- take in a list of pixel coordinates for a grid and creates a list of alternating colored squares
gPicture coors = 
    map (\(x, y) -> 
        translate x y
        $ color (col x y) 
        $ rectangleSolid (fromIntegral (gLength)) (fromIntegral(gLength))) coors
    where --adj a = fromIntegral(a - (gSize `div` 2 - gLength `div` 2)) -- adjust print coordinates to account for center origin; convert to float
          --col x y = if (mod (x+y) 2) == 0 then white else (greyN (0.8))
          isEven n = ceiling n `div` gLength `mod` 2 == 0
          col x y = if (isEven x && isEven y) || ((not (isEven x)) && not (isEven y)) then white else (greyN (0.8))


-- convert list of pictures to single picture
printPic = pictures (gPicture gPixelCoors)

----------------------------------------------------------
-- simulation
----------------------------------------------------------

-- take a previous game state and return the new game state after given time
simulateBoard :: Float -> (Board -> Board)

simulateBoard _ GameOver = GameOver

simulateBoard timeStep (Play cells)
    | length cells >= 5 = GameOver
    | otherwise = Play (Cell 1 (-9, -11) red : (concatMap updateCell cells))
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos col) = [Cell (b + 1) ((fst pos + 25), snd pos) col]

----------------------------------------------------------
main 	
 = simulate 
        (InWindow
	     "Grid" 	 -- window title
		(winSize, winSize) 	 -- window size
		(0, 0)) 	 -- window positioned in center
	white			 -- background color
    1
    initialBoard
    drawBoard
    (\view -> simulateBoard)
