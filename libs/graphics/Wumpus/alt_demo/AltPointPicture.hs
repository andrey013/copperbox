

module AltPointPicture where

import Wumpus.Alt.Geometry
import Wumpus.Alt.PointPicture

import Data.VectorSpace

square :: DPolygon 
square = Polygon 
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPolygon
funnyshape = Polygon 
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


psquare = picPolygon square
pfunnyshape = picPolygon funnyshape


demo1 = writePicture "PTsquare.ps" psquare


pic1 = psquare <> (pfunnyshape <> pfunnyshape) <> psquare

squares = psquare <> psquare <> psquare

demo2 = writePicture "PTsquares.ps" squares


-- this is shifting the origin from /zero/
-- 
rotPicture :: MPicture Polygon Double -> MPicture Polygon Double
rotPicture p = transformPicture (rotate45About (center p)) p
    

demo3 = writePicture "PTrotsquare.ps" (psquare <> rotPicture psquare <> psquare)

