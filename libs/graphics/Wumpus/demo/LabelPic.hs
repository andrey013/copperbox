
module LabelPic where

import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Extra.X11Colours

import Data.FunctionExtras ( (#) )

lbl1 :: Picture Double
lbl1 = picLabel 10 3 "Hello\nWorld" # setRGBColour aquamarine4 
                                    # setFont "Helvetica" 12



demo1 = writePicture "label1.ps" lbl1

demo2 = writePicture "label2.ps" p
  where
    p = lbl1 <..> lbl1 <..> (rotateAbout (pi/4) (center lbl1) lbl1) <..> lbl1

demo2' = writePicture "label2a.ps" p
  where
    p = (drawBounds lbl1) <..> 
        (drawBounds lbl1) <..> 
        (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) <..> 
        (drawBounds lbl1)




demo3 = writePicture "label3.ps" p
  where
    p =           (drawBounds lbl1) 
        `overlay` (drawBounds $ scale 2 2 lbl1)
        `overlay` (drawBounds $ scale 3 3 lbl1)


