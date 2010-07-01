
module Intercept where

import Wumpus.Core hiding ( blank )
import Wumpus.Extra.Base
import Wumpus.Extra.BasicObjects

import Data.List ( nub )
import Data.Maybe


test01 = intersectionPoint (ThreeLine 3 (-1) (-2)) (ThreeLine 5 (-1) 7)
test02 = pointOnLine (P2 50 50) (LS2 zeroPt (P2 100 100))
test03 = intersection line1 line2
test04 = intersectionPoint (lineSegmentToThreeLine line1) 
                           (lineSegmentToThreeLine line2)
test05 = pointOnLine (P2 50 50) line1
test06 = pointOnLine (P2 50 50) line2

test07 = boundsIntersections line1 bbox1

test08 = intersection line1 (LS2 (P2 110 90) (P2 110 110))

test09 = pointOnLine (P2 100 100) (LS2 (P2 110 90) (P2 110 110))

line1 = LS2 zeroPt     (P2 100 100)
line2 = LS2 (P2 0 100) (P2 100 0)
bbox1 = BBox (P2 90 90) (P2 110 110)


boundsIntersections :: (Fractional u, Ord u) 
                    => LineSegment u -> BoundingBox u -> [Point2 u]
boundsIntersections ln bb =
    nub $ catMaybes $ map (intersection ln) [ln1,ln2,ln3,ln4]
  where
    (bl,br,tr,tl) = corners bb
    ln1           = LS2 bl br
    ln2           = LS2 br tr
    ln3           = LS2 tr tl
    ln4           = LS2 tl bl



intersection :: (Fractional u, Ord u) 
             => LineSegment u -> LineSegment u -> Maybe (Point2 u)
intersection ln1 ln2 = intersectionPoint tl1 tl2 >>= \pt -> 
    if pointOnLine pt ln1 && pointOnLine pt ln2 then Just pt else Nothing
  where
    tl1 = lineSegmentToThreeLine ln1
    tl2 = lineSegmentToThreeLine ln2

pointOnLine :: (Num u, Ord u) => Point2 u -> LineSegment u -> Bool
pointOnLine (P2 x y) ln@(LS2 (P2 x1 y1) (P2 x2 y2)) 
    = a*x + b*y + c == 0  && x `inrange` xrange && y `inrange` yrange
  where
    ThreeLine a b c = lineSegmentToThreeLine ln
    xrange = order x1 x2
    yrange = order y1 y2

-- | Line in equational form, i.e. @Ax + By + C = 0@.
--
data ThreeLine u = ThreeLine !u !u !u 
  deriving (Eq,Show)

lineSegmentToThreeLine :: Num u => LineSegment u -> ThreeLine u
lineSegmentToThreeLine (LS2 p1 p2) = threeline p1 p2


threeline :: Num u => Point2 u -> Point2 u -> ThreeLine u
threeline (P2 x1 y1) (P2 x2 y2) = ThreeLine a b c where
    a = y1 - y2
    b = x2 - x1
    c = (x1*y2) - (x2*y1)


intersectionPoint :: Fractional u 
                  => ThreeLine u -> ThreeLine u -> Maybe (Point2 u)
intersectionPoint (ThreeLine a1 b1 c1) (ThreeLine a2 b2 c2) =
     if d==0 then Nothing else Just (P2 x y)
   where
     d   = a1*b2 - a2*b1
     c1' = negate c1
     c2' = negate c2
     x   = (b2*c1' - b1*c2') / d
     y   = (a1*c2' - a2*c1') / d