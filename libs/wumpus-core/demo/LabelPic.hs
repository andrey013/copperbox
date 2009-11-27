{-# OPTIONS -Wall #-}

module LabelPic where

import Wumpus.Core


--------------------------------------------------------------------------------



drawBounds :: (Fractional u, Ord u) => Picture u -> Picture u
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
    writeEPS_latin1 "./out/label01.eps" lbl1
    writeSVG_latin1 "./out/label01.svg" lbl1

demo02 :: IO ()
demo02 = do 
    writeEPS_latin1 "./out/label02.eps" p1
    writeSVG_latin1 "./out/label02.svg" p1
  where
    p1 = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo03 :: IO ()
demo03 = do 
    writeEPS_latin1 "./out/label03.eps" p1
    writeSVG_latin1 "./out/label03.svg" p1
  where
    p1 = (drawBounds lbl1) ->- 
         (drawBounds lbl1) ->- 
         (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
         (drawBounds lbl1)



demo04 :: IO ()
demo04 = do
    writeEPS_latin1 "./out/label04.eps" p1
    writeSVG_latin1 "./out/label04.svg" p1
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
    writeEPS_latin1 "./out/label05.eps" p1
    writeSVG_latin1 "./out/label05.svg" p1
  where
    p1 = uniformScale 10 $ stackOntoCenter [bigA, bigB] bigT


demo06 :: IO ()
demo06 = do 
    writeEPS_latin1 "./out/label06.eps" p1
    writeSVG_latin1 "./out/label06.svg" p1
  where
    p1 = hsep 20 (fn 'a') (map fn "abcdefg")
    fn = drawBounds . bigLetter peru


demo07 :: IO ()
demo07 = do 
    writeEPS_latin1 "./out/label07.eps" p1
    writeSVG_latin1 "./out/label07.svg" p1
  where
    p1 = pA ->- pB ->- pC ->- pA
    
    pA = drawBounds bigA
    pB = drawBounds $ uniformScale 2 bigB
    pC = drawBounds $ move 0 10 $ bigLetter peru 'C'


demo08 :: IO ()
demo08 = do 
    writeEPS_latin1 "./out/label08.eps" p1
    writeSVG_latin1 "./out/label08.svg" p1
  where
    p1 = hcat pA [pA, pB, pC]
    
    pA = drawBounds bigA
    pB = drawBounds $ uniformScale 2 bigB
    pC = drawBounds $ move 0 10 $ bigLetter peru 'C'

demo09 :: IO ()
demo09 = do 
    writeEPS_latin1 "./out/label09.eps" p1
    writeSVG_latin1 "./out/label09.svg" p1
  where
    p1 = (bigA `above` bigB) ->- (bigA `below` bigB) 
    
demo10 :: IO ()
demo10 = do 
    writeEPS_latin1 "./out/label10.eps" p1
    writeSVG_latin1 "./out/label10.svg" p1
  where
    p1 :: Picture Double
    p1 = frame $ textlabel () zeroPt "myst&#egrave;re"

demo11 :: IO ()
demo11 = do
    writeEPS_latin1 "./out/label11.eps" pic
    writeSVG_latin1 "./out/label11.svg" pic
  where
    pic :: Picture Double
    pic = p1 `over` p2
    p1  = multilabel plum 3 VLeft (P2 50 50) ["Hello", "from", "Wumpus"]
    p2  = bigA `at` P2 50 50


main :: IO ()
main = sequence_
  [ demo01, demo02, demo03, demo04, demo05
  , demo06, demo07, demo08, demo09, demo10
  , demo11
  ]  