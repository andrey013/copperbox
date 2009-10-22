

module LabelPic where

import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture


lbl1 :: Picture Double
lbl1 = picLabel 10 4 30 50 "Hello\nWorld"

demo1 = writePicture "label1.ps" lbl1