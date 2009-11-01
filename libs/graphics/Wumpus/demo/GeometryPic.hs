

module GeometryPic where

import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Core.PostScript
import Wumpus.Geometry.Curve
import Wumpus.Geometry.Line

import Text.PrettyPrint.Leijen

ls1 :: LineSegment Double
ls1 = lineSegment (P2 2 2) (P2 10 10)
ln1 = toLine ls1

demo1 :: Point2 Double
demo1 = lsMidpoint $ lineSegment (P2 2 2) (P2 10 10)


demo2 = toLine $ lineSegment (P2 2 2) (P2 10 10)

demo3 = writePicture "circle.ps" p1
  where
    p1 = picPath $ curvesToPath CStroke $ bezierCircle 4 (P2 10 10) 40


demo4 = writePicture "ellipse.ps" p1
  where
    p1 = picEllipse (Nothing,CStroke) 80 40 <..> picEllipse (Nothing,CFill) 10 10


