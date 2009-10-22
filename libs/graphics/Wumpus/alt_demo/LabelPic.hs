
module LabelPic where

import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture

import Wumpus.Drawing.PostScript

lbl1 :: Picture Double
lbl1 = picLabel 10 1 30 26 "Hello\nWorld"



demo1 = writePicture "label1.ps" lbl1

demo2 = writePicture "label2.ps" p
  where
    p = lbl1 <> lbl1 <> (rotateAbout (pi/4) (center lbl1) lbl1) <> lbl1

demo2' = writePicture "label2a.ps" p
  where
    p = (drawBounds lbl1) <> 
        (drawBounds lbl1) <> 
        (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) <> 
        (drawBounds lbl1)




demo3 = writePicture "label3.ps" p
  where
    p =           (drawBounds lbl1) 
        `overlay` (drawBounds $ scale 2 2 lbl1)
        `overlay` (drawBounds $ scale 3 3 lbl1)


test1 = runWumpus env0 (ps_concat 1 0 0 1 0 0)