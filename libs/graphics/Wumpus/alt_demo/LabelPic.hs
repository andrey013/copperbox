
module LabelPic where

import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture

import Wumpus.Drawing.PostScript

lbl1 :: Picture Double
lbl1 = picLabel 10 4 30 50 "Hello\nWorld"



demo1 = writePicture "label1.ps" lbl1

demo2 = writePicture "label2.ps" p
  where
    p = lbl1 <> lbl1 <> (rotatePictureAbout (pi/4) (center lbl1) lbl1) 


demo3 = writePicture "label3.ps" p
  where
    p = lbl1 `overlay` scalePicture 2 2 lbl1 `overlay` scalePicture 3 3 lbl1 


test1 = runWumpus env0 (ps_concat 1 0 0 1 0 0)