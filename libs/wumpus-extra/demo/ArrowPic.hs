

module ArrowPic where

import Wumpus.Core
import Wumpus.Extra


main :: IO ()
main = sequence_ 
  [ demo01 ]

arr1 :: Arrow Double
arr1 = arrow (P2 0 0) (P2 100 100)

arr2 :: Arrow Double
arr2 = arrowPerp (P2 10 0) (P2 110 100)

arr3 :: Arrow Double
arr3 = arrowTri (P2 20 0) (P2 120 100)

-- mkPic = overlays . map picPath

demo01 :: IO ()
demo01 = do 
    writeEPS "./out/arrow01.eps" p1 
    writeSVG "./out/arrow01.svg" p1
  where 
    p1 = picArrow arr1 `over` picArrow arr2
                       `over` picArrow arr3
