

module GeometryPic where

import Wumpus.Core
import Wumpus.Geometry


import Text.PrettyPrint.Leijen

main :: IO ()
main = sequence_
  [ demo01, demo02 ]

ls1 :: LineSegment Double
ls1 = lineSegment (P2 2 2) (P2 10 10)
ln1 = lineSegmentToLine ls1

dummy1 :: Point2 Double
dummy1 = midpoint $ lineSegment (P2 2 2) (P2 10 10)


dummy2 = lineSegmentToLine $ lineSegment (P2 2 2) (P2 10 10)


demo01 = do 
    writeEPS_latin1 "./out/geometry01.eps" p1
    writeSVG_latin1 "./out/geometry01.svg" p1
  where
    p1 :: Picture Double
    p1 = frame $ cstroke () $ curvesToPath $ bezierCircle 4 40 (P2 10 10)


demo02 = do
    writeEPS_latin1 "./out/geometry02.eps" p1
    writeSVG_latin1 "./out/geometry02.svg" p1
  where
    p1 :: Picture Double
    p1 = (frame $ ellipse () 80 40 zeroPt) ->- (frame $ ellipse () 10 10 zeroPt)


