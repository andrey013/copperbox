

module AltPicture where

import Wumpus.Alt.Geometry
import Wumpus.Alt.Picture

import Data.VectorSpace

funnyshape :: DPolygon
funnyshape = Polygon 
  [ V2 20 0, V2 0 10, V2 10 0, V2 0 10, V2 (-30) 0, V2 0 (-20) ]

square :: DPolygon 
square = Polygon 
  [ V2 40 0, V2 0 40, V2 (-40) 0, V2 0 (-40) ]

pfunnyshape = picPolygon funnyshape
psquare = picPolygon square

pic1 = psquare <> (pfunnyshape <> pfunnyshape) <> psquare

demo1 = writePicture "funnyshape.ps" pfunnyshape

demo2 = writePicture "picture1.ps" pic1



rotPicture :: MPicture Polygon Double -> MPicture Polygon Double
rotPicture p = transformPicture (rotate45About pt) $ p `move` (V2 x (0 - y/2))
  where 
    v@(V2 x y) = center p
    pt         = P2 x y
    

demo3 = writePicture "rotsquare0.ps" (psquare <> rotPicture psquare) 
