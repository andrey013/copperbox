
module LabelPic where

import Wumpus.Core
import Wumpus.Extra.X11Colours

import Data.FunctionExtras ( (#) )



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `composite` (frame $ cstroke () ph) where
    ph   = vertexPath $ corners $ boundary p



lbl1 :: Picture Double
lbl1 = picLabel 10 3 "Hello\nWorld" {- # setRGBColour aquamarine4 
                                       # setFont "Helvetica" 12 -}



demo1 = writeEPS "label1.eps" (Just ("Times-Roman",10)) lbl1

demo2 = writeEPS "label2.eps" (Just ("Times-Roman",10)) p
  where
    p = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo3 = do 
    writeEPS "label3.eps" (Just ("Courier",10)) p
    writeSVG "label3.svg" p
  where
    p = (drawBounds lbl1) ->- 
        (drawBounds lbl1) ->- 
        (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
        (drawBounds lbl1)




demo4 = writeEPS "label4.eps" (Just ("Times-Roman",10)) p
  where
    p =           (drawBounds lbl1) 
        `composite` (drawBounds $ scale 2 2 lbl1)
        `composite` (drawBounds $ scale 3 3 lbl1)


