{-# OPTIONS -Wall #-}

module LabelPic where

import Wumpus.Core
import Wumpus.Core.PictureInternal ( Picture(..) )


--------------------------------------------------------------------------------



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds PicEmpty = PicEmpty
drawBounds p        = p `over` (frame $ cstroke () ph) where
    ph   = vertexPath $ corners $ boundary p

--------------------------------------------------------------------------------


peru :: PSColour
peru = PSRgb 0.804  0.522  0.247

plum :: PSColour
plum = PSRgb 0.867  0.627  0.867

black :: PSColour
black = PSRgb 0 0 0 

lbl1 :: Picture Double
lbl1 = line1 -//- line2 where
  line1 = frame (textlabel attrs zeroPt "Hello")
  line2 = frame (textlabel attrs zeroPt "World")
  attrs = (peru, FontAttr "Helvetica" "Helvetica" 12) 


demo1 :: IO ()
demo1 = do 
    writeEPS "label1.eps" (Just ("Times-Roman",10)) lbl1
    writeSVG "label1.svg" lbl1

demo2 :: IO ()
demo2 = do 
    writeEPS "label2.eps" (Just ("Times-Roman",10)) p1
    writeSVG "label2.svg" p1
  where
    p1 = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo3 :: IO ()
demo3 = do 
    writeEPS "label3.eps" (Just ("Courier",10)) p1
    writeSVG "label3.svg" p1
  where
    p1 = (drawBounds lbl1) ->- 
         (drawBounds lbl1) ->- 
         (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
         (drawBounds lbl1)



demo4 :: IO ()
demo4 = writeEPS "label4.eps" (Just ("Times-Roman",10)) p1
  where
    p1 =        (drawBounds lbl1) 
         `over` (drawBounds $ scale 2 2 lbl1)
         `over` (drawBounds $ scale 3 3 lbl1)




bigA, bigB, bigT :: Picture Double
bigA = bigLetter black 'A'
bigB = bigLetter peru  'B'
bigT = bigLetter plum  'T'

bigLetter :: PSColour -> Char -> Picture Double
bigLetter col ch = uniformScale 5 $ frame $ textlabel attrs zeroPt [ch]
  where
    attrs = (col, FontAttr "Helvetica" "Helvetica" 12) 


-- | A should be above B, above T
demo5 :: IO ()
demo5 = do 
    writeEPS "label5.eps" (Just ("Helvetica",12)) p1
    writeSVG "label5.svg" p1
  where
    p1 = uniformScale 10 $ stackCenter [bigA, bigB, bigT]


demo6 :: IO ()
demo6 = do 
    writeEPS "label6.eps" (Just ("Helvetica",12)) p1
    writeSVG "label6.svg" p1
  where
    p1 = hsep 20 $ map (drawBounds . bigLetter peru) "abcdefg" 


demo7 :: IO ()
demo7 = do 
    writeEPS "label7.eps" (Just ("Helvetica",12)) p1
    writeSVG "label7.svg" p1
  where
    p1 = pA ->- pB ->- pC ->- pA
    
    pA = drawBounds bigA
    pB = drawBounds $ uniformScale 2 bigB
    pC = drawBounds $ move 0 10 $ bigLetter peru 'C'


demo8 :: IO ()
demo8 = do 
    writeEPS "label8.eps" (Just ("Helvetica",12)) p1
    writeSVG "label8.svg" p1
  where
    p1 = hcat [pA, pB, pC, pA]
    
    pA = drawBounds bigA
    pB = drawBounds $ uniformScale 2 bigB
    pC = drawBounds $ move 0 10 $ bigLetter peru 'C'

