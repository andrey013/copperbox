

module ArrowPic where

import Wumpus.Core
import Wumpus.Extra


main :: IO ()
main = sequence_ 
  [ demo01 ]

arr1 :: Arrow Double
arr1 = arrow zeroPt (P2 100 0)

arr2 :: Arrow Double
arr2 = arrowPerp zeroPt (P2 100 0)

arr3 :: Arrow Double
arr3 = arrowTri zeroPt (P2 100 0)

-- mkPic = overlays . map picPath

demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/arrow01.eps" pic
    writeSVG_latin1 "./out/arrow01.svg" pic
  where 
    pic :: Picture Double
    pic = vsepA VLeft 10 p1 (ps1 ++ [pb])

    p1  = arrowTri' zeroPt (P2 100 0)
    ps1 = (map picArrow [arr1, arr2, arr3 ])
    pb  = frame $ ostroke () $ bend (pi/3) (pi/3) zeroPt (P2 100 0)  