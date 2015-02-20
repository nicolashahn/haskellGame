module Board where
import Data.Monoid ((<>))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square
import Data.List
import System.Random
import Settings

-- initialization
grid = rectSquareGrid gridLength gridLength

data Board = Play Colony Colony StdGen 
          | GameOver
          deriving (Show)

type Bacteria = Int
type Position = (Int, Int)
type Colony = [Cell]

data Cell = Cell Bacteria Position Color
    deriving (Eq, Show)

initPlayerPos :: Position
initPlayerPos = (gridLength `div` 4, gridLength `div` 2)
initEnemyPos :: Position
initEnemyPos = (gridLength - (gridLength `div` 4), gridLength `div` 2)

initialBoard :: StdGen -> Board
initialBoard gen = Play
            [Cell 1 initPlayerPos green]
            [Cell 1 initEnemyPos yellow]
            gen




