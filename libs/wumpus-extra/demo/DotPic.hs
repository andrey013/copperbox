

module DotPic where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.SVGColours
import Wumpus.Geometry
import Wumpus.Geometry.CoreAdditions

main :: IO ()
main = sequence_ 
  [ demo01 ]

pt2 :: Point2 Double
pt2 = P2 100 10


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/dots01.eps" pic
    writeSVG_latin1 "./out/dots01.svg" pic
  where 
    pic :: Picture Double
    pic = extendBoundary 10 10 $ 
          uniformScale 2       $ 
            vsepA VLeft 10 p1 [p2,p3,p4,p5,p6,p7,p8,p9,p10,p11]
    
    p1  = makeDotPic (dotHLine () 1.0) points
    p2  = makeDotPic (dotVLine () 1.0) points
    p3  = makeDotPic (dotCircle () 1.0) points
    p4  = makeDotPic (dotAsterisk () 1.0) points   
    p5  = makeDotPic (dotStar () 1.0) points
    p6  = makeDotPic (dotOPlus () 1.0) points
    p7  = makeDotPic (dotOCross () 1.0) points
    p8  = makeDotPic (dotX () 1.0) points
    p9  = makeDotPic (dotPlus () 1.0) points
    p10 = makeDotPic (dotDiamond () 1.0) points
    p11 = makeDotPic (dotSquare () 1.0) points



points :: [Point2 Double]
points = [P2 0 0, P2 32 10, P2 64 0, P2 96 10]

makeDotPic :: (Fractional u, Ord u) 
           => (Point2 u -> Picture u) -> [Point2 u] -> Picture u
makeDotPic fn xs = multi $ dashline : map fn xs
  where
    dashline = frame $ ostroke attr $ vertexPath xs
    attr     = (cadetBlue, DashPattern $ evenDashes 1)