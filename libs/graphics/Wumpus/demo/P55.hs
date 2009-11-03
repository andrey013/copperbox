


module P55 where

import Wumpus.Core
import Wumpus.Geometry

-- Note 
-- Processing has (0,0) at top left, 
-- PostScript has (0,0) at bottom left
-- hence the @scale 1 (-1)@

test01 = writeEPS "P55ex1.eps" Nothing (scale 1 (-1) $ example1)

example1 :: Picture Double
example1 = stack [diagonals 40 90, diagonals 60 62, diagonals 20 40]

diagonals :: Int -> Int -> Picture Double
diagonals x y = picMultiPath $ map lineSegmentToPath [l1,l2,l3] where
  l1 = mkLine x      y (x+20) (y-40)
  l2 = mkLine (x+10) y (x+30) (y-40)
  l3 = mkLine (x+20) y (x+40) (y-40)


mkLine :: Fractional u => Int -> Int -> Int -> Int -> LineSegment u
mkLine x1 y1 x2 y2 = fmap realToFrac $ lineSegment (P2 x1 y1) (P2 x2 y2)
 