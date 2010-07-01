{-# OPTIONS -Wall #-}


module P55 where

import Wumpus.Core
import Wumpus.Extra
import Wumpus.Extra.PictureLanguage

import System.Directory

-- Note 
-- Processing has (0,0) at top left, 
-- PostScript has (0,0) at bottom left
-- hence the @scale 1 (-1)@

main :: IO ()
main = do 
     createDirectoryIfMissing True "./out/"
     sequence_ [ test01 ]


test01 :: IO ()
test01 = do 
   writeEPS_latin1 "./out/P55ex1.eps" example1
   writeSVG_latin1 "./out/P55ex1.svg" example1

coordChange :: (Num u, Ord u) => Picture u -> Picture u
coordChange = scale 1 (-1)

rect100 :: Picture Double
rect100 = frame $ fillPolygon light_gray $ square 100 zeroPt
  where
    light_gray = psColour $ Gray (0.75::Double)

example1 :: Picture Double
example1 = coordChange $ ls `stackOnto` rect100 where
  ls = [diagonals 40 90, diagonals 60 62, diagonals 20 40]

diagonals :: Int -> Int -> Picture Double
diagonals x y = multi $ map (frame . zostroke . lineSegmentToPath) [l1,l2,l3]
  where
    l1 = mkLine x      y (x+20) (y-40)
    l2 = mkLine (x+10) y (x+30) (y-40)
    l3 = mkLine (x+20) y (x+40) (y-40)


mkLine :: Fractional u => Int -> Int -> Int -> Int -> LineSegment u
mkLine x1 y1 x2 y2 = fmap realToFrac $ lineSegment (P2 x1 y1) (P2 x2 y2)
 