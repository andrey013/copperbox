

module GeometryPic where

import Wumpus.Core.Geometry
import Wumpus.Core.OutputPostScript
import Wumpus.Core.Picture
import Wumpus.Core.PictureLanguage
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


demo3 = writeEPS "geom3.eps" Nothing p1
  where
    p1 = frame $ cstroke () $ curvesToPath $ bezierCircle 4 (P2 10 10) 40


demo4 = writeEPS "geom4.eps" Nothing p1
  where
    p1 = (frame $ ellipse () zeroPt 80 40) ->- (frame $ ellipse () zeroPt 10 10)


