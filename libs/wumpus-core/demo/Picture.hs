

module Picture where

import Wumpus.Core

import Data.VectorSpace





square :: DPicture 
square = frame $ cstroke () $ vertexPath
  [ P2 0 0, P2 40 0, P2 40 40, P2 0 40 ]

funnyshape :: DPicture
funnyshape = frame $ cstroke () $ vertexPath
  [ P2 0 0, P2 20 0, P2 20 10, P2 30 10, P2 30 20, P2 0 20 ]



demo1 = writePS "picture1.ps" Nothing [funnyshape ->- square]



pic1 = square ->- (funnyshape ->- funnyshape) ->- square

squares = square ->- square ->- square

demo2 = do 
   writePS  "picture2.ps"  Nothing [squares]
   writeSVG "picture2.svg" squares 
    

demo3 = do 
    writeEPS "picture3.eps" Nothing p1 
    writeSVG "picture3.svg" p1
  where     
    p1 = square ->- (rotate45About (center squares) squares) ->- square


demo4 = do 
    writeEPS "picture4.eps" Nothing p1
    writeSVG "picture4.svg" p1
  where
    p1 = square -//- squares
   

demo5 = writeEPS "picture5.eps" Nothing p1
  where
    p1 = square `composite` (rotate (pi/4) squares)
   

demo6 = writeEPS "picture6.eps" Nothing p1
  where
    p1 = square `composite` (rotate45 square)


demo7 = writeEPS "picture7.eps" Nothing p1
  where
    p1 = square -@- (rotate45 square)


