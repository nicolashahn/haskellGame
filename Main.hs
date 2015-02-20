module Main where
import qualified Board as B
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Settings

-- the window's height/width in pixels
winSize :: Int
winSize = gridLength * cellSize

main
 = do   gen <- getStdGen
        play (InWindow "Grid" (winSize, winSize) (0, 0))
             white
             1
             (B.initialBoard gen)
             B.drawBoard 
             B.handleInput
 
