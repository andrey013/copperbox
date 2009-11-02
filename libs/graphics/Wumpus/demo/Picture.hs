

module Picture where

import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.OutputPostScript
import Wumpus.Core.Picture
import Wumpus.Core.PictureLanguage
import Wumpus.Extra.Polygon

import Data.FunctionExtras ( (#) )
import Data.VectorSpace

import Text.PrettyPrint.Leijen ( pretty )


square :: DPolygon 
square = Polygon
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPolygon
funnyshape = Polygon
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


psquare = picPolygon CStroke square
pfunnyshape = picPolygon CFill funnyshape

test01 = pretty $ movePic (V2 100 40) psquare
test02 = pretty $ rotate45 psquare

demo0 = writePS "picture0.ps" Nothing [pfunnyshape]

demo1 = writePS "picture1.ps" Nothing [psquare]




pic1 = psquare ->- (pfunnyshape ->- pfunnyshape) ->- psquare

psquares = psquare ->- psquare ->- psquare

demo2 = writePS "picture2.ps" Nothing [psquares]

    

demo3 = writeEPS "picture3.eps" Nothing p1 
  where     
    p1 = psquare ->- (drawFrame $ rotate45About (center psquares) psquares) ->- psquare

d3 = pretty $ psquare ->- (rotate45About (center psquares) psquares) ->- psquare

demo4 = writeEPS "picture4.eps" Nothing p1
  where
    p1 = psquare -//- (psquares # setRGBColour (RGB3 1 0 1))
   
d4 = pretty $ psquare -//- (psquares # setRGBColour (RGB3 1 0 1))

demo5 = writeEPS "picture5.eps" Nothing p1
  where
    p1 = psquare `composite` (rotatePicture (pi/4) psquares)
   

demo6 = writeEPS "picture6.eps" Nothing  p1 
  where
    p1 = picPolygon CStroke $ regularPolygon 6 50


demo7 = writeEPS "picture7.eps" Nothing p1
  where
    p1 = psquare `composite` (rotate45 psquare)


demo8 = writeEPS "picture8.eps" Nothing p1
  where
    p1 = psquare -@- (rotate45 psquare)


