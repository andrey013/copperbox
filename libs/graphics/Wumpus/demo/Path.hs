{-# OPTIONS -Wall #-}

module Path where

import Wumpus.Core.Curve
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Wumpus.Drawing.Path
import Wumpus.Drawing.Picture
import Wumpus.Drawing.X11Colours


import Prelude hiding ( abs )

demo1 :: IO ()
demo1 = writePicture "path1.ps" drawing1 where
  drawing1 = displace 60 480 $ withFont (timesRoman 9)  (oline <//> opath)
  oline    = withRgbColour maroon0 $ picPath stroke $ 
               straightLine $ lineS1 zeroPt
  opath    = picPath stroke $ htildev (P2 0 60)
           
lineS1 :: DPoint2 -> DLineSegment2
lineS1 = hline 60


htildev :: DPoint2 -> Path Double
htildev = \pt -> newPath pt `lineTo` (hvec 10 :: Vec2 Double) 
                        {-    `curveTo` (p1,p2,p3)  -}
                            `lineTo` (vvec 20 :: Vec2 Double)
--  where
--   (Curve _ p1 p2 p3) = tildeCurve 40 


