-- main.hs

import Data.Monoid ((<>))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Data.List
import System.Random
import System.Random.Shuffle

------------------------------------------------------------------------------
-- Initialization --
------------------------------------------------------------------------------

gridLength :: Int
gridLength = 30 -- length of grid

cellSize :: Int
cellSize = 25 -- cell's pixel height/width
cellFloat :: Float
cellFloat = fromIntegral cellSize

winSize :: Int
winSize = gridLength * cellSize -- the windows' height/width in pixels
winFloat :: Float
winFloat = fromIntegral winSize

grid = rectSquareGrid gridLength gridLength

data Board = Play Colony Colony StdGen 
          | GameOver String
          deriving (Show)

type Bacteria = Int
type Position = (Int, Int)
type Colony = [Cell]

data Cell = Cell Bacteria Position Color
    deriving (Eq, Show)

colorP = green :: Color
colorE = red :: Color

initPlayerPos :: Position
initPlayerPos = (gridLength `div` 4, gridLength `div` 4)
initEnemyPos :: Position
initEnemyPos = (gridLength - (gridLength `div` 4), gridLength - (gridLength `div` 4))

initialBoard :: StdGen -> Board
initialBoard gen = Play
            [Cell 1 initPlayerPos green]
            [Cell 1 initEnemyPos red]
            gen

------------------------------------------------------------------------------
-- Game state --
------------------------------------------------------------------------------

-- helper fn
drawColony cells = pictures [makeSquare x y col <> showNum n x y
                            | Cell n (x, y) col <- cells]

drawBoard :: Board -> Picture
drawBoard (GameOver t)
    = scale 0.3 0.3
    $ translate (-winFloat + 50 ) (0.0)
    $ color red
    $ text t

drawBoard (Play cellsP cellsE gen) 
    = pictures [printGrid, drawColony cellsP, drawColony cellsE]
        where
        printGrid = pictures (gridSquares $ indices grid)

------------------------------------------------------------------------------
-- Simulation --
------------------------------------------------------------------------------

-- TODO: shuffle list before checking
-- pick a position that each bacteria could spawn
randPos :: [Position] -> Bacteria -> StdGen -> (Maybe Position, StdGen)
randPos [] _ gen  = (Nothing, gen)
randPos (p:ps) b gen 
    | b > randNum = (Just p, newGen)
    | otherwise = randPos ps b newGen
    where (randNum, newGen) = randomR(1, 9) gen 

-- possibly grow cells and update existing cells
grow :: Colony -> [Maybe Position] -> Color -> Colony
grow [] _ _ = []
grow _ [] _ = []
grow (c@(Cell pop xy colr):cs) (Just p:ps) colrNew
    = (Cell 1 p colrNew) : (Cell (pop - (1)) xy colr) : grow cs ps colr
grow (c:cs) (Nothing:ps) colrNew
    = c : grow cs ps colrNew

-- pick position to spawn at every index
pickSpawns :: [[Position]] -> [Bacteria] -> StdGen -> [Maybe Position]
pickSpawns [] _ _ = []
pickSpawns (p:ps) (b:bs) gen 
    = spawnPos : pickSpawns ps bs newGen
    where
        (spawnPos, newGen) = if (length p) > 0 then randPos (shuffle' p (length p) gen) b gen
                                                else randPos [] b gen



-- list of list of places bacteria could spawn. each list within a list corresponds to an
-- index in the colony that the bacteria would spawn from 
spawnPotential :: Colony -> Colony -> [[Position]]
spawnPotential c1 c2 = map (\\ filledCells) adjC 
    where filledCells = (colonyPos c1 ++ colonyPos c2)
          adjC = adjCells (colonyPos c1)


-- list of list of all neighbors of colony's bacteria 
adjCells :: [Position] -> [[Position]]
adjCells [] = []
adjCells ps = map (neighbours grid) ps

-- increases size of colony
growColony :: Colony -> Colony -> Color -> StdGen -> Colony 
growColony c1 c2 colr gen 
    = grow c1 chosenSpawns colr
    where 
        chosenSpawns = pickSpawns (spawnPotential c1 c2) popList gen
        popList = map cellPop c1
--
-- updates population of one cell 
upCellPop :: Cell -> Cell
upCellPop c@(Cell pop xy col) = if pop < 10
                              then (Cell (pop + (1)) xy col)
                              else c

-- update list of bacteria's population
updateCells :: Colony -> Colony
updateCells [] = []
updateCells cells = map upCellPop cells

-- take a previous game state and return the new game state after given time
simulateBoard :: Float -> (Board -> Board)
simulateBoard _ (GameOver t) = (GameOver t)
simulateBoard timeStep (Play colonyP colonyE gen)
    | (length colonyP) + (length colonyE) >= (gridLength * gridLength) = GameOver (
        if (length colonyP) > (length colonyE) 
            then "Player Wins: " ++ (show (length colonyP)) ++ " cells"
            else "Enemy Wins: " ++ (show (length colonyE)) ++ " cells"
        )
    | otherwise = Play 
                  (fullUpdate colonyP colonyE colorP genP) 
                  (fullUpdate colonyE colonyP colorE genE) 

                  genNew
    where
        (genThis, genNew) = split gen
        (genP, genE) = split genThis
        fullUpdate c1 c2 colr g = (growColony (updateCells c1) c2 colr g)

------------------------------------------------------------------------------
-- Event handling --
------------------------------------------------------------------------------
handleEvents :: Event -> Board -> Board
handleEvents _ (GameOver t) = (GameOver t)
handleEvents (EventKey (MouseButton LeftButton) Down _ _)
             (Play cellsP cellsE gen)
    | length cellsP >= 20 = GameOver "Game over"
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
    $ scale (0.1) (0.1)
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
        play (InWindow "Grid" (winSize, winSize) (0, 0))     -- window positioned in center
             white
             10
             (initialBoard gen)
             drawBoard
             handleEvents
             simulateBoard