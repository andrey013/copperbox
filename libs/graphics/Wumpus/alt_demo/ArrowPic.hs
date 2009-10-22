

module ArrowPic where

import Wumpus.Alt.Arrow
import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture


arr1 :: Arrow Double
arr1 = arrow (P2 0 0) (P2 100 100)

arr2 :: Arrow Double
arr2 = arrowPerp (P2 10 0) (P2 110 100)

arr3 :: Arrow Double
arr3 = arrowTri (P2 20 0) (P2 120 100)

-- mkPic = overlays . map picPath

demo1 = writePicture "arrow1.ps" (picArrow arr1 `overlay` picArrow arr2
                                                `overlay` picArrow arr3)
