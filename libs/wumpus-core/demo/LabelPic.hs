{-# OPTIONS -Wall #-}

module LabelPic where

import Wumpus.Core
import Wumpus.Core.PictureInternal ( Picture(..) )


--------------------------------------------------------------------------------



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds p        = p `over` (frame $ cstroke () ph) where
    ph   = vertexPath $ corners $ boundary p

--------------------------------------------------------------------------------


peru :: PSRgb
peru = RGB3 0.804  0.522  0.247

plum :: PSRgb
plum = RGB3 0.867  0.627  0.867

black :: PSRgb
black = RGB3 0 0 0 



lbl1 :: Picture Double
lbl1 = line1 -//- line2 where
  line1 = frame (textlabel attrs zeroPt "Hello")
  line2 = frame (textlabel attrs zeroPt "World")
  attrs = (peru, FontAttr "Helvetica" "Helvetica" SVG_REGULAR 12) 


demo01 :: IO ()
demo01 = do 
    writeEPS "./out/label01.eps" (Just ("Times-Roman",10)) lbl1
    writeSVG "./out/label01.svg" lbl1

demo02 :: IO ()
demo02 = do 
    writeEPS "./out/label02.eps" (Just ("Times-Roman",10)) p1
    writeSVG "./out/label02.svg" p1
  where
    p1 = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo03 :: IO ()
demo03 = do 
    writeEPS "./out/label03.eps" (Just ("Courier",10)) p1
    writeSVG "./out/label03.svg" p1
  where
    p1 = (drawBounds lbl1) ->- 
         (drawBounds lbl1) ->- 
         (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
         (drawBounds lbl1)



demo04 :: IO ()
demo04 = do
    writeEPS "./out/label04.eps" (Just ("Times-Roman",10)) p1
    writeSVG "./out/label04.svg" p1
  where
    p1 =        (drawBounds lbl1) 
         `over` (drawBounds $ scale 2 2 lbl1)
         `over` (drawBounds $ scale 3 3 lbl1)




bigA, bigB, bigT :: Picture Double
bigA = bigLetter black 'A'
bigB = bigLetter peru  'B'
bigT = bigLetter plum  'T'

bigLetter :: PSRgb -> Char -> Picture Double
bigLetter col ch = uniformScale 5 $ frame $ textlabel attrs zeroPt [ch]
  where
    attrs = (col, FontAttr "Helvetica" "Helvetica" SVG_REGULAR 12) 


-- | A should be above B, above T
demo05 :: IO ()
demo05 = do 
    writeEPS "./out/label05.eps" (Just ("Helvetica",12)) p1
    writeSVG "./out/label05.svg" p1
  where
    p1 = uniformScale 10 $ stackOntoCenter [bigA, bigB] bigT


demo06 :: IO ()
demo06 = do 
    writeEPS "./out/label06.eps" (Just ("Helvetica",12)) p1
    writeSVG "./out/label06.svg" p1
  where
    p1 = hsep 20 (fn 'a') (map fn "abcdefg")
    fn = drawBounds . bigLetter peru


demo07 :: IO ()
demo07 = do 
    writeEPS "./out/label07.eps" (Just ("Helvetica",12)) p1
    writeSVG "./out/label07.svg" p1
  where
    p1 = pA ->- pB ->- pC ->- pA
    
    pA = drawBounds bigA
    pB = drawBounds $ uniformScale 2 bigB
    pC = drawBounds $ move 0 10 $ bigLetter peru 'C'


demo08 :: IO ()
demo08 = do 
    writeEPS "./out/label08.eps" (Just ("Helvetica",12)) p1
    writeSVG "./out/label08.svg" p1
  where
    p1 = hcat pA [pA, pB, pC]
    
    pA = drawBounds bigA
    pB = drawBounds $ uniformScale 2 bigB
    pC = drawBounds $ move 0 10 $ bigLetter peru 'C'

demo09 :: IO ()
demo09 = do 
    writeEPS "./out/label09.eps" (Just ("Helvetica",12)) p1
    writeSVG "./out/label09.svg" p1
  where
    p1 = (bigA -//- bigB) ->- (bigA -\\- bigB) 
    


main :: IO ()
main = sequence_
  [ demo01, demo02, demo03, demo04, demo05
  , demo06, demo07, demo08, demo09
  ]  