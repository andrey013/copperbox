

module ArrowPic where

import Wumpus.Alt.Arrow
import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture

arr0 = arrowheadVee 10 (pi/4)

arr1 = arrow (P2 0 0) (P2 100 100)

mkPic = overlays . map picPath

demo1 = writePicture "arrow1.ps" (mkPic arr1)
