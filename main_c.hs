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

data Board = Play Colony Colony StdGen Turn
          | GameOver String
          deriving (Show)

type Bacteria = Int
type Position = (Int, Int)
type Colony = [Cell]
type Turn = Int

data Cell = Cell Bacteria Position Color
    deriving (Eq, Show)

type Both = (Colony, Colony) 

colorP = green :: Color
colorE = red :: Color

initPlayerPos :: Position
-- initPlayerPos = (gridLength `div` 4, gridLength `div` 2)
initPlayerPos = (gridLength - (gridLength `div` 4), gridLength `div` 2)
initEnemyPos :: Position
-- initEnemyPos = (gridLength - (gridLength `div` 4), gridLength `div` 2)
initEnemyPos = (gridLength `div` 4, gridLength - (gridLength `div` 2))
initialBoard :: StdGen -> Board
initialBoard gen = Play
            [Cell 1 initPlayerPos green]
            [Cell 1 initEnemyPos red]
            gen
            0

------------------------------------------------------------------------------
-- Game state --
------------------------------------------------------------------------------

-- helper fn
drawColony cells = pictures [makeSquare x y col <> showNum n x y
                            | Cell n (x, y) col <- cells]

drawBoard :: Board -> Picture
drawBoard (GameOver t)
    = scale 0.2 0.2
    $ translate (-winFloat ) (0.0)
    $ color red
    $ text t

drawBoard (Play cellsP cellsE gen turn) 
    = pictures [printGrid, drawColony cellsP, drawColony cellsE]
        where
        printGrid = pictures (gridSquares $ indices grid)

------------------------------------------------------------------------------
-- Simulation --
------------------------------------------------------------------------------

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
grow (c@(Cell pop xy colr):cs) (Just p:ps) colrBase
    | colr == colrBase = (Cell 1 p colrBase) : (Cell (pop - 3) xy colrBase) : grow cs ps colrBase
    | otherwise        = (Cell pop xy colrBase) : grow cs ps colrBase
grow (c@(Cell pop xy colr):cs) (Nothing:ps) colrBase
    = (Cell pop xy colrBase) : grow cs ps colrBase

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
spawnPotential c1 c2 = (map (\\ filledCells) adjC )
    where filledCells = (colonyPos c1 ++ colonyPos c2)
          adjC = adjPositions (colonyPos c1)


-- list of list of all neighbors of colony's bacteria 
adjPositions :: [Position] -> [[Position]]
adjPositions [] = []
adjPositions ps = map (neighbours grid) ps

-- increases size of colony
growColony :: Colony -> Colony -> Color -> StdGen -> Colony 
growColony c1 c2 colr gen = grow c1 chosenSpawns colr
    where 
        chosenSpawns = ( pickSpawns (spawnPotential c1 c2) popList gen )
        popList = (map cellPop c1)
--
-- updates population of one cell 
upCellPop :: Color -> Cell -> Cell
upCellPop baseColor c@(Cell pop xy col) = if pop < 10 && col == baseColor
                                          then (Cell (pop + (1)) xy col)
                                          else c

-- update list of bacteria's population
updateCells :: Colony -> Color -> Colony
updateCells [] _ = []
updateCells cells baseColor = map (upCellPop baseColor) cells

----------------------------
--  fighting
----------------------------

-- opposite of upCellPop
-- color changes for debug purposes
decCellPop :: (Cell, Int) -> Cell
decCellPop ((Cell pop xy col), r) 
    | r > pop =  (Cell (pop - 1) xy (mixColors 1 1 col yellow))
    | otherwise = (Cell pop xy (mixColors 1 1 col yellow))

-- returns list of cells from a colony that match the list of positions
matchPositions :: Colony -> [Position] -> [Cell]
matchPositions [] _ = []
matchPositions _  [] = []
matchPositions colony positions = concatMap (\p -> (filter (\c -> cellPos c == p) colony)) positions

-- which cells in colony are neighbors of the given single cell
adjCells :: Colony -> Cell -> [Cell]
adjCells [] _ = []
adjCells colony cell = matchPositions colony (neighbours grid (cellPos cell))

-- returns a list of colony's cells and the number of opponent cells they're adjacent to 
getFightCells :: Colony -> Colony -> [(Cell, Int)]
getFightCells [] _ = []
getFightCells _ [] = []
getFightCells colony opponent = nub $ map (\x -> (x, freq x adjList)) adjList
    where
        adjList = concatMap (\x -> adjCells colony x) opponent

-- take list of ALL of one colony's cells, list of cells that are fighting
-- returns list of all colony cells after being decremented/killing cells
decCells :: Colony -> [(Cell, Int)] -> StdGen -> Colony
decCells [] _ _ = []
decCells colony [] _ = colony
decCells colony fightCells gen  = map decCellPop fightCellsUpdate ++ (colony \\ c)
    where
        randNums = take (length fightCells) $ randomRs (1, 10) gen :: [Int]
        fightCellsUpdate = zip c $ zipWith (+) randNums $ n 
        (c, n) = unzip fightCells

-- remove all cells that have population < 1
killCells :: Colony -> Colony
killCells [] = []
killCells c = if length c > 1
                then filter (\x -> (cellPop x) < cap) remove0
                else remove0
            where remove0 = (filter (\x -> (cellPop x)> 0) c)
                  cap = if length c > 50 then 3 else 5
--
-- takes player and enemy colonies and returns a tuple of both colonies
-- decrement population and/or remove from colony (kill) cells that are fighting 
-- (adjacent cells from differing colonies)
fight :: Colony -> Colony -> StdGen -> (Colony, Colony)
fight [] [] _ = ([],[])
fight p [] _ = (p,[])
fight [] e _ = ([],e)
fight p e gen = (killCells (decCells p fightCellsP genP), killCells (decCells e fightCellsE genE))
    where
        fightCellsP = getFightCells p e
        fightCellsE = getFightCells e p
        (genP, genE) = split gen

-- take a previous game state and return the new game state after given time
simulateBoard :: Float -> (Board -> Board)
simulateBoard _ (GameOver t) = (GameOver t)
simulateBoard timeStep (Play colonyP colonyE gen turn)
    -- | (length colonyP) + (length colonyE) >= (gridLength * gridLength) = GameOver (
    | length colonyP < 1 = GameOver (
        if (length colonyP) > (length colonyE) 
            then "Player Wins: " ++ (show (length colonyP)) ++ " cells after "++(show turn)++" turns"
            else "Enemy Wins: " ++ (show (length colonyE)) ++ " cells after "++(show turn)++" turns"
        )
    | length colonyE < 1 = GameOver (
        if (length colonyP) > (length colonyE) 
            then "Player Wins: " ++ (show (length colonyP)) ++ " cells after "++(show turn)++" turns"
            else "Enemy Wins: " ++ (show (length colonyE)) ++ " cells after "++(show turn)++" turns"
        )
    | otherwise = Play 
                  (fst f)
                  (snd f)
                  genNew
                  (turn + 1)
    where
        f = (fight (fullUpdate colonyP colonyE colorP genP) (fullUpdate colonyE colonyP colorE genE) genThis)
        (genThis, genNew) = split gen
        (genP, genE) = split genThis
        fullUpdate c1 c2 colr g = (growColony (updateCells c1 colr) c2 colr g)

------------------------------------------------------------------------------
-- Event handling --
------------------------------------------------------------------------------
handleEvents :: Event -> Board -> Board
handleEvents _ (GameOver t) = (GameOver t)
handleEvents (EventKey (MouseButton LeftButton) Down _ _)
             (Play cellsP cellsE gen turn)
    | length cellsP >= 2000 = GameOver "Game over"
    | otherwise = Play (Cell 1 (0, 0) blue : (concatMap updateCell cellsP)) (Cell 1 (0, 10) yellow : concatMap updateCell cellsE) gen turn
    where
        updateCell :: Cell -> [Cell]
        updateCell c@(Cell b pos col) = [Cell (b + 1) ((fst pos + 1), snd pos) col]

handleEvents _ board = board  -- all other possible events

------------------------------------------------------------------------------
-- Helper functions --
------------------------------------------------------------------------------
-- Get frequency of element in a list
freq :: Eq a => a -> [a] -> Int
freq x [] = 0
freq x (y:ys)
    | x == y    = 1 + (freq x ys)
    | otherwise = freq x ys

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
cellPos (Cell _ xy _) = xy

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
             20 
             (initialBoard gen)
             drawBoard
             handleEvents
             simulateBoard
