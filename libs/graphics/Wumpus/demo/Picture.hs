

module Picture where

import Wumpus.Core.Geometry
import Wumpus.Core.Picture
import Wumpus.Extra.Polygon

import Data.VectorSpace

square :: DPolygon 
square = Polygon
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPolygon
funnyshape = Polygon
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


psquare = picPolygon CStroke square
pfunnyshape = picPolygon CFill funnyshape

demo0 = writePicture "funnyshape.ps" pfunnyshape

demo1 = writePicture "square.ps" psquare


pic1 = psquare <> (pfunnyshape <> pfunnyshape) <> psquare

psquares = psquare <> psquare <> psquare

demo2 = writePicture "squares.ps" psquares

    

demo3 = writePicture "rotsquare.ps" p1 
  where     
    p1 = psquare <> (rotatePicture (pi/4) psquares) <> psquares


demo4 = writePicture "abovesquare.ps" p1
  where
    p1 = psquare </> psquares 
   
demo5 = writePicture "oversquares.ps" p1
  where
    p1 = psquare `overlay` (rotatePicture (pi/4) psquares)
   

demo6 = writePicture "hexagon.ps" p1 
  where
    p1 = picPolygon CStroke $ regularPolygon 6 50

