
module GridPic where

import Wumpus.Core.Geometry
import Wumpus.Core.OutputPostScript
import Wumpus.Core.Picture
import Wumpus.Extra.Grid


grid1 :: Grid
grid1 = Grid xs where
  xs = [ nodeAt (1,0) "Gal(M)"
       , nodeAt (0,1) "delta"
       , nodeAt (1,1) "Gal(N/M)"
       , nodeAt (0,2) "(E/E')"
       ]
       


nodeAt :: (Int,Int) -> String -> PlacedNode
nodeAt (x,y) name = (NamedNode name, P2 x y) 


demo1 = writePicture "grid1.ps" pic1 where
  pic1 = nodePicture $ nodeMap 100 50 grid1

