-- sim.hs
-- sim test

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
--import Data.Random
--import Data.Random.Source.DevRandom
--import Data.Random.Extras
import System.Random
import Data.List

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

theGrid = rectSquareGrid gLength gLength

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


-- probability that a bacteria will grow
probGrowth = 0.5

playerCells :: [Cell]
playerCells = [Cell 1 (1,1) blue]
enemyCells :: [Cell]
enemyCells = [Cell 1 (gLength-1, gLength-1) red]
foodCells :: [Cell]
foodCells = [Cell 1 (5,5) green]

playerPos :: [Cell] -> [Position]
playerPos cells = map cellPos cells

enemyPos :: [Position]
enemyPos = map cellPos enemyCells
foodPos :: [Position]
foodPos = map cellPos foodCells

-- all cells that have something in them
-- filledCells :: [Position]
-- filledCells = playerPos ++ enemyPos

-- just to get a cell's population
cellPop :: Cell -> Int
cellPop (Cell p _ _) = p

-- get cell's position
cellPos :: Cell -> (Int, Int)
cellPos (Cell _ xy _) = xy

cellCol :: Cell -> Color
cellCol (Cell _ _ c) = c


-- uses probGrowth to increment population by 0 or 1
--randZeroOne :: RandomGen g => g -> Int
--randZeroOne x = if (fst (randomR (0,1) x) ) <= probGrowth then 0 else 1

--above is broken for now

-- updates population of one cell
upCellPop :: Cell -> Cell
upCellPop (Cell pop xy col) = if pop < 9  
                                -- replace the 1 below with a randZeroOne when it works
                                then (Cell (pop + (1)) xy col)
                                else (Cell pop xy col)

growCells :: [Cell] -> [Cell]
growCells cells = combine borderPos ++ cells 
    where borderPos = borderCells (playerPos cells)

combine :: [Position] -> [Cell]
combine [] = []
combine borderPos = (Cell 1 (head borderPos) red) : (combine $ tail borderPos)

-- update list of bacteria's population
upCellsPop :: [Cell] -> [Cell]
upCellsPop [] = []
upCellsPop xs = map upCellPop xs

--takes list of cell positions
--returns list of adjacent positions
adjCells :: [Position] -> [Position]
adjCells [] = []
adjCells ps = nub (foldl (\a p -> a ++ (neighbours theGrid p)) [] ps)

-- checks a list of bacteria, uses population and positions
-- returns a list of possible empty cells to spawn a new bacteria
borderCells :: [Position] -> [Position]
borderCells [] = []
borderCells ps = adjCells ps \\ filledCells

-- currently does NOT get a random element
-- gets the middle element from a list
randElem :: [a] -> a
--randElem [] = Nothing
randElem cs = (cs !! (div (length cs) 2))


-- spawn one cell using a random empty border cell
-- pass it in one team's bacteria, gives the same list + 1 more bacteria
spawnCell :: Color -> [Cell] -> [Cell]
spawnCell _ [] = []
spawnCell col cs = (Cell 1 (randElem (borderCells (map cellPos cs))) col):cs



updateCells :: [Cell] -> [Cell]
updateCells [] = []
updateCells (c:cs) = spawnCell col (upCellsPop (c:cs))
            where col = cellCol c

-------------------------------------
-- random stuff
-------------------------------------

--getRand = do
--    gen <- getStdGen
--    randZeroOne gen


-- take a previous game state and return the new game state after given time
simulateBoard :: Float -> (Board -> Board)

simulateBoard _ GameOver = GameOver

simulateBoard timeStep (Play cells)
    | length cells >= 20 = GameOver
    | otherwise = Play (growCells $ updateCells cells)


----------------------------------------------------------
main 	
 = simulate 
        (InWindow
	     "Grid" 	 -- window title
		(winSize, winSize) 	 -- window size
		(0, 0)) 	 -- window positioned in center
	white			 -- background color
    20
    initialBoard
    drawBoard
    (\view -> simulateBoard)
