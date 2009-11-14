
module GridPic where

import Wumpus.Core
import Wumpus.Extra


main :: IO ()
main = sequence_ [ demo01 ]

grid1 :: Grid
grid1 = Grid xs where
  xs = [ nodeAt (1,0) "Gal(M)"
       , nodeAt (0,1) "delta"
       , nodeAt (1,1) "Gal(N/M)"
       , nodeAt (0,2) "(E/E')"
       ]
       


nodeAt :: (Int,Int) -> String -> PlacedNode
nodeAt (x,y) name = (NamedNode name, P2 x y) 


demo01 = do 
    writeEPS "./out/grid01.eps" (Just ("Courier", 12)) pic1 
    writeSVG "./out/grid01.svg" pic1 
  where
    pic1   = nodePicture (nodeMap 100 50 grid1) no_pic
    
    no_pic = blankPicture (BBox zeroPt zeroPt)
