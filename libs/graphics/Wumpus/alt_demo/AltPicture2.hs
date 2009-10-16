

module AltPicture2 where

import Wumpus.Alt.Geometry
import Wumpus.Alt.PictureZ

import Data.VectorSpace

square :: DPolygon 
square = Polygon 
  [ V2 0 0, V2 40 0, V2 40 40, V2 0 40 ]

funnyshape :: DPolygon
funnyshape = Polygon 
  [ V2 0 0, V2 20 0, V2 20 10, V2 30 10, V2 30 20, V2 0 20 ]


psquare = picPolygon square
pfunnyshape = picPolygon funnyshape

pic1 = psquare <> (pfunnyshape <> pfunnyshape) <> psquare


demo1 = writePicture "square.ps" psquare

squares = psquare <> psquare <> psquare

demo2 = writePicture "squares.ps" pic1

-- this is shifting the origin from /zero/
-- 
rotPicture :: MPicture Polygon Double -> MPicture Polygon Double
rotPicture p = (transformPicture (rotate45About pt) p) `move` (V2 x (0 - y/2))
  where 
    v@(V2 x y) = center p
    pt         = P2 x y
    

demo3 = writePicture "rotsquare.ps" (psquare `overlay` rotPicture psquare)