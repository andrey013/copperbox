

module ArrowPic where

import Wumpus.Core
import Wumpus.Extra


main :: IO ()
main = sequence_ 
  [ demo01 ]

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/arrow01.eps" pic
    writeSVG_latin1 "./out/arrow01.svg" pic
  where 
    pic :: Picture Double
    pic = vsepA VLeft 10 p1 [p2,p3]

    p1  = arrowTri' zeroPt pt2
    p2  = arrowHook' zeroPt pt2
    p3  = arrowPerp' zeroPt pt2

