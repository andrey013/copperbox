

module Grid1 where

import Wumpus.Core.Instances
import Wumpus.Core.Point
import Wumpus.Core.PostScript

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Grid
import Wumpus.Drawing.X11Colours

demo1 :: IO ()
demo1 = writePS "grid1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; setRgbColour dodgerBlue1 
                ; mapM_ drawLine $ grid (P2 0 0) (P2 5 4) 
                }
