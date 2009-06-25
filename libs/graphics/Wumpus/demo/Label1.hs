

module Label1 where

import Wumpus.Core.Frame
import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.PostScript

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.X11Colours


demo1 :: IO ()
demo1 = writePS "label1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { setupFont "Times-Roman" 15
                ; ps_translate 60 380 
                ; setRgbColour seaGreen4 
                ; drawLine $ LS (P2 0 0) (P2 50 40) 
                ; drawText (P2 0 0)  "text1"
                }
