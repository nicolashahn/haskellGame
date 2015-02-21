-- main.hs
 
import Data.Monoid ((<>))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
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
 
friendlyColor :: Color
friendlyColor = blue
 
enemyColor :: Color
enemyColor = red
 
data Board = Play CellBoard
          | GameOver
          deriving (Eq, Show)
 
type Population = Int
type Position = (Int, Int)
type CellBoard = [Cell]
 
data Cell = Cell Population Position Color
    deriving (Eq, Show)
 
------------------------------------------------------------------------------
-- Cell Functions --
------------------------------------------------------------------------------
 
-- just to get a cell's population
cellPop :: Cell -> Int
cellPop (Cell population _ _) = population
 
--This function returns a cell with one more population than the inputed cell
growCellPop :: Cell -> Cell
growCellPop (Cell population pos color) = (Cell (population+1) pos color)
 
--This funciton returns a cell with the same values as the input, but a new color
setCellColor :: Cell -> Color -> Cell
setCellColor (Cell population pos color) newColor = (Cell population pos newColor)
 
--This function grows a cell into the given player's cell with one population.
-- Used for cell captures
growNewCell :: Cell -> Color -> Cell
growNewCell (Cell _ pos _) newColor = (Cell 1 pos newColor)
-- get cell's position
cellPos :: Cell -> Position
cellPos (Cell _ pos _) = pos
 
-- get cell's Color
cellColor :: Cell -> Color
cellColor (Cell _ _ color) = color
 
--takes a cell and the current board returns a list of that cell's neighbors
--Input: Current cell, Cell array of the board
--Output: list of neighbors of that cell
getCellNeighbors :: CellBoard ->Cell-> [Cell]
getCellNeighbors currentBoard (Cell _ cellPosition _) = checkBoardForCells currentBoard (getAdjacentCoordinates cellPosition)
 
--Takes a list of coordinates and returns a list of cells that match those coordinates.
--Coordinates that do not exist are not recovered.
checkBoardForCells :: CellBoard -> [Position] -> [Cell]
checkBoardForCells [] coordinateList = []
checkBoardForCells (thisCell:restOfCells) coordinateList =   if (elem (cellPos thisCell) coordinateList)
                                                            then
                                                                --if the position is in the list of coordinates, add the cell to the output list
                                                                [thisCell] ++ checkBoardForCells restOfCells coordinateList
                                                            else
                                                                --otherwise, skip this cell and keep looking.
                                                                checkBoardForCells restOfCells coordinateList
------------------------------------------------------------------------------
-- Assorted utility functions --
------------------------------------------------------------------------------
 
--this function take a coordinate and returns a list of that coordinate's neighbors
getAdjacentCoordinates :: Position -> [Position]
getAdjacentCoordinates (xPos, yPos) = [(xPos+1, yPos),(xPos, yPos+1),(xPos, yPos-1),(xPos-1, yPos)]
 
--Creates a board of the given size where the player spawns at (max)
testBoard :: CellBoard
testBoard = createBoard [1..25]
 
createBoard :: [Int] -> CellBoard
createBoard dimentions = foldl (++) [] (map (createRow (last dimentions)) dimentions)
 
--Takes an integer and the width of the row and returns a row of cells with those proportions
createRow :: Int -> Int -> [Cell]
createRow rowWidth row = map (createCellInRow rowWidth row) [1..rowWidth]
 
--Creates an individual cell
--takes y-coordinate x-coordinate and max coordinate
--returns a cell with the given coordinates
--If the cell is at (max/2, 2), creates a player cell
--If the cell is at (max/2, max-2), creates an enemy cell.
--Otherwise creates a black (empty) cell
createCellInRow :: Int -> Int -> Int -> Cell
createCellInRow maxPos yPos xPos
                    | (xPos == (maxPos/2)) && (yPos == 2)           = (Cell 1 (xPos, yPos) blue)
                    | (xPos == (maxPos/2)) && (yPos == (maxPos-2))  = (Cell 1 (xPos, yPos) blue)
                    | otherwise                                     = (Cell 0 (xPos, yPos) black)
 
 
randomGenerator :: IO StdGen
randomGenerator = getStdGen
 
--Should return a random float from 0 to 1
randomNumGen :: IO Double
randomNumGen = getStdRandom (randomR (0.0, 1.0))
 
--Checks to see if the given number is greater than a random number between 0 and 1
compareToRandom :: Double ->  IO Bool
compareToRandom compareMe = do
                            randomNumber <- randomNumGen
                            return (compareMe > randomNumber)
 
------------------------------------------------------------------------------
-- Game Logic --
------------------------------------------------------------------------------
 
 
 
updateBoard :: CellBoard -> CellBoard
updateBoard oldBoard =          fightCells
                                (updatePlayer enemyColor
                                (updatePlayer friendlyColor oldBoard))
 
updatePlayer :: Color -> CellBoard -> CellBoard
updatePlayer playerColor oldBoard =  moveCells playerColor
                                     (growCells playerColor oldBoard)
 
--This function calls for the current player's cells to be updated
-- then it calls for the border cells to be grown
growCells :: Color -> CellBoard -> CellBoard
growCells playerColor oldBoard =    popNewCells playerColor
                                    (growCurrentCells playerColor oldBoard)
 
 
------------------------------------------------------------------------------
-- Cell Fighting --
------------------------------------------------------------------------------
 
--For now, simply returns the old board
--Runs a check on each cell to see if there are any adjacent enemies.
-- Each colony gets to fight and to retaliate. Therefore two colonies
-- next to eachother will both go through the fight algorithm twice.
fightCells :: CellBoard -> CellBoard
fightCells oldBoard = oldBoard
 
 
 
------------------------------------------------------------------------------
-- Moving Player's Cells --
------------------------------------------------------------------------------
 
--For now, simply returns the old board
moveCells :: Color -> CellBoard -> CellBoard
moveCells playerColor oldBoard = oldBoard
 
 
------------------------------------------------------------------------------
-- Growing the active player's borders--
------------------------------------------------------------------------------
 
--This function takes the board and the player's color and
-- returns a new board where that player's border cells have grown
popNewCells :: Color -> CellBoard -> [Cell]
popNewCells playerColor oldBoard = map (growThisBorderCell playerColor oldBoard) oldBoard
 
--returns the old cell if growth failed, the cell already belonged to a player,
-- or if the cell doesnt have any neighboring cells belonging to the player
-- otherwise returns a cell that may or may not have grown into a player's cell
growThisBorderCell :: Color -> CellBoard -> Cell -> Cell
growThisBorderCell playerColor currentBoard oldCell= do
                                                        condition <- compareToRandom (colonizeEquation playerColor currentBoard oldCell)
                                                    if (
                                                        --Makes sure that the cell doesnt belong to either player
                                                        cellColor oldCell == friendlyColor || cellColor oldCell == enemyColor
                                                        --Makes sure that the formula for colonization returns true (a value of false means
                                                         -- the old cell is returned)
                                                        || not condition
                                                    )
                                                    then
                                                    return (oldCell)
                                                    else
                                                    return (growNewCell oldCell playerColor)
--Retuns true if the formula for colonization returns a greater value than the
-- random generated number (from 0 to 1)
--Control the probability of colonization by changing colonizationValue
colonizeCondition :: Color -> CellBoard -> Cell -> IO Bool
colonizeCondition playerColor currentBoard thisCell = compareToRandom (colonizeEquation playerColor currentBoard thisCell)
 
colonizeEquation :: Color -> CellBoard -> Cell -> Double
colonizeEquation playerColor currentBoard thisCell = (2)/(1 + (exp 1)^(colonizationValue * fromIntegral(countNumberOfNeighbors playerColor currentBoard thisCell))) - 1
 
--A number closer to 0 will yield less probability of colonization
colonizationValue :: Float
colonizationValue = -0.02
 
--This function counts the number of neighbors an empty cell has.
-- Returns 0 if there are no neighbors of the player's color or if
-- the cell is already owned by a player.
countNumberOfNeighbors :: Color -> CellBoard -> Cell -> Int
countNumberOfNeighbors playerColor currentBoard currentCell = countNumberofCells playerColor (getCellNeighbors currentBoard currentCell)
 
 
--This function takes a list of cells and a player color, and returns the total population
-- of the cells owned by the player
countNumberofCells :: Color -> [Cell] -> Int
countNumberofCells playerColor cellList = foldl (+) 0 (map (countPopulationOfCell playerColor) cellList)
 
--This function returns the number of bacteria in a cell owned by the player
countPopulationOfCell :: Color -> Cell -> Int
countPopulationOfCell playerColor currentCell = if (cellColor currentCell == playerColor)
                                                    then
                                                    cellPop currentCell
                                                    else
                                                    0
 
 
------------------------------------------------------------------------------
-- Growing the active player's colony --
------------------------------------------------------------------------------
 
 
--This function maps a function onto each cell on the board to grow a player's cells
-- within the colony
growCurrentCells :: Color -> CellBoard -> CellBoard
growCurrentCells playerColor oldBoard = map (growThisCell playerColor) oldBoard
 
--Grows the current cell if it is the active player's color
growThisCell :: Color -> Cell -> Cell
growThisCell playerColor oldCell =      
                                        do
                                            condition <- growCondition oldCell
                                        if ((cellColor oldCell == playerColor) && condition)
                                        then
                                        --If the player color is the active player
                                        -- update the cell's population
                                            growCellPop oldCell
                                        else
                                            oldCell
 
 
--Retuns true if the formula for growth returns a greater value than the
-- random generated number (from 0 to 1)
growCondition :: Cell -> IO Bool
growCondition thisCell = compareToRandom (growEquation thisCell)
 
growEquation :: Cell -> Double
growEquation thisCell = (2/(1 + (exp 1)^(growthValue * fromIntegral(cellPop thisCell))) - 1)
 
--A number closer to 0 will yield less probability of growth
growthValue :: Float
growthValue = -0.05
 
------------------------------------------------------------------------------
-- Main --
------------------------------------------------------------------------------
main    
 = print updateBoard testBoard