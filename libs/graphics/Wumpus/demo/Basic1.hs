{-# OPTIONS -Wall #-}

module Basic1 where

import Wumpus.Core.Wumpus
import Wumpus.Drawing.Basic

import Wumpus.Core.Point

demo1 :: IO ()
demo1 = writePS "basic1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { translate 60 380 
                ; line (0,0) (0,150)
                ; line (0,0) (150,0)
                ; circle (5,5) 15
                ; setgray 0.5
                ; disk (60,5) 15
                ; wedge (100,5) 15 0 60
                ; setgray 0 
                ; ellipse (60,5) (15,10)
                ; ellipticarc (120,5) (15,10) 0 270
                ; polygon [(140,5), (140,30), (170,5), (170,30)]
                ; diamond (10,50) (30,30)
                ; disk (10,50) 2
                ; diamond2 (80,50) (30,20)
                ; setrgbcolor 1 0 0
                ; plusDot (P2 150 50)
                }
