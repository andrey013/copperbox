{-# OPTIONS -Wall #-}

module LabelPic where

import Wumpus.Core
import Wumpus.Core.PictureInternal ( Picture(..) )


--------------------------------------------------------------------------------



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `over` (frame $ cstroke () ph) where
    ph   = vertexPath $ corners $ boundary p

--------------------------------------------------------------------------------


peru :: PSColour
peru = PSRgb 0.804  0.522  0.247

lbl1 :: Picture Double
lbl1 = line1 -//- line2 where
  line1 = frame (textlabel attrs zeroPt "Hello")
  line2 = frame (textlabel attrs zeroPt "World")
  attrs = (peru, FontAttr "Helvetica" "Helvetica" 12) 


demo1 :: IO ()
demo1 = writeEPS "label1.eps" (Just ("Times-Roman",10)) lbl1

demo2 :: IO ()
demo2 = writeEPS "label2.eps" (Just ("Times-Roman",10)) p
  where
    p = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo3 :: IO ()
demo3 = do 
    writeEPS "label3.eps" (Just ("Courier",10)) p
    writeSVG "label3.svg" p
  where
    p = (drawBounds lbl1) ->- 

        (drawBounds lbl1) ->- 
        (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
        (drawBounds lbl1)



demo4 :: IO ()
demo4 = writeEPS "label4.eps" (Just ("Times-Roman",10)) p
  where
    p =           (drawBounds lbl1) 
        `over` (drawBounds $ scale 2 2 lbl1)
        `over` (drawBounds $ scale 3 3 lbl1)




bigA, bigB :: Picture Double
bigA = mkBigLetter 'A' (PSRgb 0 0 0)
bigB = mkBigLetter 'B' peru


mkBigLetter :: Char -> PSColour -> Picture Double
mkBigLetter ch col = uniformScale 5 $ frame $ textlabel attrs zeroPt [ch]
  where
    attrs = (col, FontAttr "Helvetica" "Helvetica" 12) 

demo5 :: IO ()
demo5 = do 
    writeEPS "label5.eps" (Just ("Helvetica",12)) p
    writeSVG "label5.svg" p
  where
    p = bigA `over` bigB 
