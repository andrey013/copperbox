

module ArrowPic where

import Wumpus.Core
import Wumpus.Extra


main :: IO ()
main = sequence_ 
  [ demo01 ]

point2 :: Point2 Double
point2 = P2 100 10

arr1 :: Arrow Double
arr1 = arrow zeroPt point2

arr2 :: Arrow Double
arr2 = arrowPerp zeroPt point2

arr3 :: Arrow Double
arr3 = arrowTri zeroPt point2

-- mkPic = overlays . map picPath

demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/arrow01.eps" pic
    writeSVG_latin1 "./out/arrow01.svg" pic
  where 
    pic :: Picture Double
    pic = vsepA VLeft 10 p1 (ps1 ++ [ph])

    p1  = arrowTri' zeroPt point2
    ps1 = (map picArrow [arr1, arr2, arr3 ])
    ph  = arrowHook' zeroPt point2


