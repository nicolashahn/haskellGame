-- prints grid to display
--
import Graphics.Gloss
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

gLength = 30 -- length of grid
gSize = gLength * gLength
grid = rectSquareGrid gLength gLength
gCoors = indices grid


-- converts Grid coordinates to pixel coordinates for printing purposes
coorsToPixels [] = []
coorsToPixels coors = (fromIntegral(fst headCoor * gLength), fromIntegral(snd headCoor * gLength)) : coorsToPixels (tail coors)
        where headCoor = head coors
 
-- list of pixel coordinates
gPixelCoors = coorsToPixels gCoors

-- take in a list of pixel coordinates for a grid and creates a list of alternating colored squares
gPicture []  = []
gPicture coors = (translate fHead sHead $ color col $ rectangleSolid (fromIntegral(gLength)) (fromIntegral(gLength))) : gPicture (tail coors)
        where fHead = adj $ fst $ head coors
              sHead = adj $ snd $ head coors
              -- adj coor: adjusts print coordinates to account for center origin, converts to float 
              adj coor = fromIntegral(coor - (gSize `div` 2 - gLength `div` 2))
              isEven n = ceiling n `div` gLength `mod` 2 == 0
              col = if (isEven fHead && isEven sHead) || ((not (isEven fHead)) && not (isEven sHead)) then blue else azure

printPic = pictures (gPicture gPixelCoors)

main 	
 = display 
        (InWindow
	     "Grid" 	 -- window title
		(gSize, gSize) 	 -- window size
		(0, 0)) 	 -- window positioned in center
	white			 -- background color
    printPic
    -- picture to display

