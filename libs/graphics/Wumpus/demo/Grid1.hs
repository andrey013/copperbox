{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}


module Grid1 where

import Wumpus.Core.Frame
import Wumpus.Core.Instances
import Wumpus.Core.Point
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Grid
import Wumpus.Drawing.X11Colours

--
import Wumpus.Test.TypeRestrict
import Wumpus.Core.VSExtra
import Wumpus.Core.Matrix

import Data.VectorSpace

demo1 :: IO ()
demo1 = writePS "grid1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; setRgbColour dodgerBlue1 
                ; mapM_ drawLine $ oldGrid (P2 0 0) (P2 5 4) 
                ---
                ; ps_translate 0 100
                ; setRgbColour dodgerBlue4
                ; mapM_ drawLine $ grid 10 10 (P2 50 40) origin 
                }
  frame1 = Frame2 (P2 0 0) (V2 1 0) (V2 0 0.5)
  origin = P2 0 0


-- dummy1 = steps 4 10.0