{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Core



peru :: PSRgb
peru = RGB3 0.804  0.522  0.247

plum :: PSRgb
plum = RGB3 0.867  0.627  0.867

black :: PSRgb
black = RGB3 0 0 0 


square :: DPicture 
square = frame $ cstroke () $ vertexPath
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPicture
funnyshape = frame $ cstroke () $ vertexPath
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


demo01 :: IO ()
demo01 = do 
  writePS_latin1 "./out/picture01.ps"    [funnyshape ->- square]
  writeSVG "./out/picture01.svg" $ funnyshape ->- square


pic1 :: Picture Double
pic1 = square ->- (funnyshape ->- funnyshape) ->- square

squares :: Picture Double
squares = square ->- square ->- square

demo02 :: IO ()
demo02 = do 
   writePS_latin1 "./out/picture02.ps"  [squares]
   writeSVG "./out/picture02.svg" squares 
    

demo03 :: IO ()
demo03 = do 
    writeEPS_latin1"./out/picture03.eps" p1 
    writeSVG "./out/picture03.svg" p1
  where     
    p1 = square ->- (rotate45About (center squares) squares) ->- square


demo04 :: IO ()
demo04 = do 
    writeEPS_latin1"./out/picture04.eps" p1
    writeSVG "./out/picture04.svg" p1
  where
    p1 = square -//- squares
   

demo05 :: IO ()
demo05 = do 
    writeEPS_latin1"./out/picture05.eps" p1
    writeSVG "./out/picture05.svg" p1
  where
    p1 = square `over` (rotate (pi/4) squares)
   

demo06 :: IO ()
demo06 = do 
    writeEPS_latin1"./out/picture06.eps" p1
    writeSVG "./out/picture06.svg" p1
  where
    p1 = square `over` (rotate45 square)


-- Note the move via @at@ is not apparent when SVG file is 
-- viewed with Mozilla or Chrome - check picture7a.svg
-- We only see that the move has /worked/ when we compose
-- with with `over` a square at the origin. 

demo07 :: IO ()
demo07 = do 
    writeEPS_latin1"./out/picture07.eps" p1
    writeSVG "./out/picture07.svg" p1
    writeSVG "./out/picture07a.svg" p2
  where
    p1 = square `over` p2
    p2 = (square `at` (P2 100 30))  -@- (rotate45 square)


demo08 :: IO ()
demo08 = do 
    writeEPS_latin1"./out/picture08.eps" p1
    writeSVG "./out/picture08.svg" p1
  where
    p1 = hspace 20 square square

mkFilledSquare :: (PSColour c, Fill c) => c -> DPicture 
mkFilledSquare col = frame $ fill col $ vertexPath
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]


demo09 :: IO ()
demo09 = do 
    writeEPS_latin1"./out/picture09.eps" p1
    writeSVG "./out/picture09.svg" p1
  where
    p1 = (alignH HTop s1 s2) `op` s3
    s1 = uniformScale 1.5  $ mkFilledSquare plum 
    s2 = uniformScale 1.75 $ mkFilledSquare peru
    s3 = scale 3 1.5       $ mkFilledSquare black
    op = alignH HBottom
 

demo10 :: IO ()
demo10 = do 
    writeEPS_latin1 "./out/picture10.eps" p1
    writeSVG "./out/picture10.svg" p1
  where
    p1 = vsepA VRight 5 s1 [s2,s3]
    s1 = uniformScale 1.5  $ mkFilledSquare plum 
    s2 = uniformScale 1.75 $ mkFilledSquare peru
    s3 = scale 3 1.5       $ mkFilledSquare black
 


main :: IO ()
main = sequence_
  [ demo01, demo02, demo03, demo04, demo05
  , demo06, demo07, demo08, demo09, demo10
  ]