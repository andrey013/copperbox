

module CDiagram where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Frame
import Wumpus.Core.Geometric
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Polygon
import Wumpus.Core.Line
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.PostScript


demo1 :: IO ()
demo1 = writePicture "cdiagram1.ps" drawing1 where
  drawing1 = displace 60 480 $ withFont (timesRoman 15) $ diagram1
               
 

diagram1 :: Picture
diagram1 = picLines arrs <..> cat labels



arrs :: [DLineSegment2]
arrs = concat $ sequence [f1,f2,f3,f4] $ boundingBox $ square 100 zeroPt 
  where
    f1 = veeArrow . expandLine (0.8) . hline   100  . northWest
    f2 = veeArrow . expandLine (0.8) . vline (-100) . northWest
    f3 = veeArrow . expandLine (0.8) . hline   100  . southWest
    f4 = veeArrow . expandLine (0.8) . vline (-100) . northEast


-- P B 
-- A C
labels :: [Picture]
labels = zipWith fn ["A","C","B","P"] (extractPoints $ square 100 zeroPt)
  where
    fn c pt@(P2 x y) = displace (x-5) (y-5) $ picLabel c 10 10


