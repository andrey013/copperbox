{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

module Grid where

import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Grid
import Wumpus.Drawing.X11Colours



demo1 :: IO ()
demo1 = writePicture "grid1.ps" drawing1 where
  drawing1  = displace 60 380 $ 
                grid1 <..> (displace 200 0 grid2) <..> (displace 300 0 triangles)
  
  grid1     = withRgbColour dodgerBlue1 $ grid 20 20 100 80
                    
  grid2     = grid 10 10 50 40

  triangles = multiput 6 (V2 10 0) dotTriangle


{-
calgrid :: [LineBag]
calgrid = zipWith fn (calendarGrid 0 30) (repeat dotX) where
  fn :: DVec2 -> LineBag -> LineBag
  fn (V2 x y) df = pointwise (translate (x*30) (y*30)) df
-}


