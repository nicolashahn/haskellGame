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

type Bacteria = Int
type Position = (Float, Float)

data Cell = Cell Bacteria Position
    deriving (Eq, Show)

initialBoard :: Board
initialBoard = Play
            [ Cell 1 (0.0, 0.0) ] -- Default bacteria

drawBoard :: Board -> Picture
drawBoard GameOver
    = scale 0.5 0.5
    $ translate (-400.0) (0.0)
    $ color red
    $ text  "GAME OVER"

drawBoard (Play cells) 
    = pictures [printPic, colony]
    where
    colony = pictures [translate x y (color black (scale 0.2 0.2 $ text (show n)))
                      | Cell n (x, y) <- cells]

-----------------------------------------------------------
-- print grid
-----------------------------------------------------------
gLength = 25 -- length of grid
gSize = gLength * gLength
grid = rectSquareGrid gLength gLength
gCoors = indices grid

-- converts Grid coordinates to pixel coordinates for printing purposes
coorsToPixels coors = map (\(x, y) -> (convert x, convert y)) coors
    where convert a = fromIntegral(a * gLength)

-- list of pixel coordinates
gPixelCoors = coorsToPixels gCoors

-- take in a list of pixel coordinates for a grid and creates a list of alternating colored squares
gPicture coors = map (\(x, y) -> translate (adj x) (adj y) $ color (col (adj x) (adj y)) $ rectangleSolid (fromIntegral (gLength)) (fromIntegral(gLength))) coors
    where adj a = fromIntegral(a - (gSize `div` 2 - gLength `div` 2)) -- adjust print coordinates to account for center origin; convert to float
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
    | otherwise = Play (Cell 1 (-9.0, -11.0) : (concatMap updateCell cells))
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos) = [Cell (b + 1) ((fst pos + 25), snd pos)]

----------------------------------------------------------
main 	
 = simulate 
        (InWindow
	     "Grid" 	 -- window title
		(gSize, gSize) 	 -- window size
		(0, 0)) 	 -- window positioned in center
	white			 -- background color
    1
    initialBoard
    drawBoard
    (\view -> simulateBoard)
