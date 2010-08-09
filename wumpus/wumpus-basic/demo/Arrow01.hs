{-# OPTIONS -Wall #-}

module Arrow1 where

import Wumpus.Basic.Arrowheads
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr

import Wumpus.Core                      -- package: wumpus-core

import System.Directory




main :: IO ()
main = createDirectoryIfMissing True "./out/"
    >> writeEPS_latin1 "./out/arrow01.eps" pic1
    >> writeSVG_latin1 "./out/arrow01.svg" pic1 


pic1 :: Picture Double
pic1 = drawGraphicU $ 
           tri90 (standardAttr 18) 0 (P2 0 0)
         . tri60 (standardAttr 18) 0 (P2 10 0)
         . tri45 (standardAttr 18) 0 (P2 20 0)
         . textline (textAttr $ standardAttr 18) "sample" (P2 30 (-4))
         . otri90 (standardAttr 18) 0 (P2 100 0)
         . otri60 (standardAttr 18) 0 (P2 110 0)
         . otri45 (standardAttr 18) 0 (P2 120 0)
         
