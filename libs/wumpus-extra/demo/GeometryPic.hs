

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
dummy1 = lsMidpoint $ lineSegment (P2 2 2) (P2 10 10)


dummy2 = lineSegmentToLine $ lineSegment (P2 2 2) (P2 10 10)


demo01 = do 
    writeEPS "./out/geometry01.eps" Nothing p1
    writeSVG "./out/geometry01.svg" p1
  where
    p1 = frame $ cstroke () $ curvesToPath $ bezierCircle 4 (P2 10 10) 40


demo02 = do
    writeEPS "./out/geometry02.eps" Nothing p1
    writeSVG "./out/geometry02.svg" p1
  where
    p1 = (frame $ ellipse () zeroPt 80 40) ->- (frame $ ellipse () zeroPt 10 10)


