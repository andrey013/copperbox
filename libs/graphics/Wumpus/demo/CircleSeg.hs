


module CircleSeg where

import Wumpus.Core.Curve
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import qualified Wumpus.Drawing.Basic as B
import Wumpus.Drawing.X11Colours



demo1 :: IO ()
demo1 = writePS "circleseg1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 150 380 
                ; B.setRgbColour tomato4
                ; B.drawPolygon $ B.dotDiamond zeroPt
                ; B.drawLine $ hline zeroPt 70
                ; B.drawLine $ pointwise (rotateAbout (pi/4) zeroPt) $ hline zeroPt 70
                ; B.setRgbColour steelBlue1
                ; B.drawBezier $ pointwise (uniformScale 60) $ circleSegment (pi/4)
                }

