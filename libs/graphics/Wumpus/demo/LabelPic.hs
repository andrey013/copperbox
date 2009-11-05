
module LabelPic where

import Wumpus.Core.BoundingBox hiding ( center )
import Wumpus.Core.Geometry
import Wumpus.Core.OutputPostScript
import Wumpus.Core.OutputSVG
import Wumpus.Core.Picture
import Wumpus.Core.PictureLanguage
import Wumpus.Extra.X11Colours

import Data.FunctionExtras ( (#) )



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `composite` (picPath CStroke path) where
    bb   = extractBounds p
    path = straightLinePath $ corners bb



lbl1 :: Picture Double
lbl1 = picLabel 10 3 "Hello\nWorld" {- # setRGBColour aquamarine4 
                                       # setFont "Helvetica" 12 -}



demo1 = writeEPS "label1.eps" (Just ("Times-Roman",10)) lbl1

demo2 = writeEPS "label2.eps" (Just ("Times-Roman",10)) p
  where
    p = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo2' = do 
    writeEPS "label2a.eps" (Just ("Times-Roman",10)) p
    writeSVG "label2a.svg" p
  where
    p = (drawBounds lbl1) ->- 
        (drawBounds lbl1) ->- 
        (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
        (drawBounds lbl1)




demo3 = writeEPS "label3.eps" (Just ("Times-Roman",10)) p
  where
    p =           (drawBounds lbl1) 
        `composite` (drawBounds $ scale 2 2 lbl1)
        `composite` (drawBounds $ scale 3 3 lbl1)


