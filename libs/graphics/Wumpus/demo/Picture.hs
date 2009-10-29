

module Picture where

import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Extra.Polygon

import Data.FunctionExtras ( (#) )
import Data.VectorSpace

import Text.PrettyPrint.Leijen


square :: DPolygon 
square = Polygon
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPolygon
funnyshape = Polygon
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


psquare = picPolygon CStroke square
pfunnyshape = picPolygon CFill funnyshape

test01 = pretty $ move (V2 100 40) psquare
test02 = pretty $ rotate45 psquare

demo0 = writePicture "picture0.ps" pfunnyshape

demo1 = writePicture "picture1.ps" psquare


pic1 = psquare <..> (pfunnyshape <..> pfunnyshape) <..> psquare

psquares = psquare <..> psquare <..> psquare

demo2 = writePicture "picture2.ps" psquares

    

demo3 = writePicture "picture3.ps" p1 
  where     
    p1 = psquare <..> (drawFrame $ rotate45About (center psquares) psquares) <..> psquare

d3 = pretty $ psquare <..> (rotate45About (center psquares) psquares) <..> psquare

demo4 = writePicture "picture4.ps" p1
  where
    p1 = psquare <||> (psquares # setRGBColour (RGB3 1 0 1))
   
d4 = pretty $ psquare <||> (psquares # setRGBColour (RGB3 1 0 1))

demo5 = writePicture "picture5.ps" p1
  where
    p1 = psquare `overlay` (rotatePicture (pi/4) psquares)
   

demo6 = writePicture "picture6.ps" p1 
  where
    p1 = picPolygon CStroke $ regularPolygon 6 50


demo7 = writePicture "picture7.ps" p1
  where
    p1 = psquare `overlay` (rotate45 psquare)