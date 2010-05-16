{-# OPTIONS -Wall #-}


module ShapesPic where

import Wumpus.Core  hiding ( center )
import Wumpus.Extra hiding ( rectangle )
import Wumpus.Extra.Shapes

-- Note 
-- Processing has (0,0) at top left, 
-- PostScript has (0,0) at bottom left
-- hence the @scale 1 (-1)@

main :: IO ()
main = sequence_ [ test01 ]


test01 :: IO ()
test01 = do 
   writeEPS_latin1 "./out/Shapes1.eps" picture1
   writeSVG_latin1 "./out/Shapes1.svg" picture1



picture1 :: Picture Double
picture1 = rect1 ->- rect2
  where
    rect1 = frame $ strokeRectangle $ r1

    rect2 = frame $ strokeRectangle $ rotate45About (center r1) r1

    r1    = rectangle 100 50 (P2 50 25)