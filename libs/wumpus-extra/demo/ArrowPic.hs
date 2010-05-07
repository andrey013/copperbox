

module ArrowPic where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours

main :: IO ()
main = sequence_ 
  [ demo01 ]

pt2 :: Point2 Double
pt2 = P2 100 10

std_attr :: (RGB3 Double,Double)
std_attr = (black, 1.0)

demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/arrow01.eps" pic
    writeSVG_latin1 "./out/arrow01.svg" pic
  where 
    pic :: Picture Double
    pic = backgroundGrid (cornflowerBlue, [LineWidth 0.5])  10 $ 
            vsepA VLeft 10 p1 [p2,p3,p4,p5,p6] -//- frameMulti [bend1, bend2]
               -//- strokedCrossedRect () () 40 20
    p1  = arrowTri' std_attr zeroPt pt2
    p2  = arrowHook' std_attr zeroPt pt2
    p3  = arrowPerp' std_attr zeroPt pt2
    p4  = arrowBracket' std_attr zeroPt pt2
    p5  = arrowOutBracket' std_attr zeroPt pt2
    p6  = arrowStrokedCurved' std_attr zeroPt pt2

    bend1 = ostroke () $ curveToPath $ bend (d2r 45) (d2r 135) zeroPt (P2 100 0)
    bend2 = ostroke () $ curveToPath $ bend (d2r 45) (d2r 135) zeroPt (P2 0 (-50))