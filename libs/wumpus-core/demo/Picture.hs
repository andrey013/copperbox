{-# OPTIONS -Wall #-}

module Picture where

import Wumpus.Core

square :: DPicture 
square = frame $ cstroke () $ vertexPath
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPicture
funnyshape = frame $ cstroke () $ vertexPath
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]


demo1 :: IO ()
demo1 = do 
  writePS "picture1.ps" Nothing [funnyshape ->- square]
  writeSVG "picture1.svg" $ stack [funnyshape ->- square]


pic1 :: Picture Double
pic1 = square ->- (funnyshape ->- funnyshape) ->- square

squares :: Picture Double
squares = square ->- square ->- square

demo2 :: IO ()
demo2 = do 
   writePS  "picture2.ps"  Nothing [squares]
   writeSVG "picture2.svg" squares 
    

demo3 :: IO ()
demo3 = do 
    writeEPS "picture3.eps" Nothing p1 
    writeSVG "picture3.svg" p1
  where     
    p1 = square ->- (rotate45About (center squares) squares) ->- square


demo4 :: IO ()
demo4 = do 
    writeEPS "picture4.eps" Nothing p1
    writeSVG "picture4.svg" p1
  where
    p1 = square -//- squares
   

demo5 :: IO ()
demo5 = do 
    writeEPS "picture5.eps" Nothing p1
    writeSVG "picture5.svg" p1
  where
    p1 = square `over` (rotate (pi/4) squares)
   

demo6 :: IO ()
demo6 = do 
    writeEPS "picture6.eps" Nothing p1
    writeSVG "picture6.svg" p1
  where
    p1 = square `over` (rotate45 square)


-- Note the move via @at@ is not apparent when SVG file is 
-- viewed with Mozilla or Chrome - check picture7a.svg
-- We only see that the move has /worked/ when we compose
-- with with `over` a square at the origin. 

demo7 :: IO ()
demo7 = do 
    writeEPS "picture7.eps" Nothing p1
    writeSVG "picture7.svg" p1
    writeSVG "picture7a.svg" p2
  where
    p1 = square `over` p2
    p2 = (square `at` (P2 100 30))  -@- (rotate45 square)


demo8 :: IO ()
demo8 = do 
    writeEPS "picture8.eps" Nothing p1
    writeSVG "picture8.svg" p1
  where
    p1 = hspace 20 square square
