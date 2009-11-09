

module ArrowPic where

import Wumpus.Core
import Wumpus.Extra.Arrow


arr1 :: Arrow Double
arr1 = arrow (P2 0 0) (P2 100 100)

arr2 :: Arrow Double
arr2 = arrowPerp (P2 10 0) (P2 110 100)

arr3 :: Arrow Double
arr3 = arrowTri (P2 20 0) (P2 120 100)

-- mkPic = overlays . map picPath

demo1 :: IO ()
demo1 = do 
    writeEPS "arrow1.eps" Nothing p1 
    writeSVG "arrow1.svg" p1
  where 
    p1 = picArrow arr1 `composite` picArrow arr2
                       `composite` picArrow arr3
