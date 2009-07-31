{-# OPTIONS -Wall #-}

module Path where

import Wumpus.Core.Curve
import Wumpus.Core.Line hiding (lineTo)
import Wumpus.Core.Path
import Wumpus.Core.Point

import Wumpus.Drawing.Basic
import Wumpus.Drawing.X11Colours


import Prelude hiding ( abs )

demo1 :: IO ()
demo1 = writePicture "path1.ps" drawing1 where
  drawing1 = displace 60 480 $ withFont (timesRoman 9)  (oline <//> opath)
  oline    = withRgbColour maroon0 $ picLine $ lineS1 zeroPt
  opath    = picPath $ htildev (P2 0 60)
           
lineS1 :: DPoint2 -> DLineSegment2
lineS1 = hline 60


htildev :: DPoint2 -> DPath
htildev = \pt -> newPath pt `lineTo` (hline 10) `curveTo` (tildeCurve 40) 
                            `lineTo` (vline 20)



