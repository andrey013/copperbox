{-# OPTIONS -Wall #-}

module Path where

import Wumpus.Core.Curve
import Wumpus.Core.Line hiding (lineTo)
import Wumpus.Core.Path
import Wumpus.Core.Point

import Wumpus.Drawing.Basic
import Wumpus.Drawing.Label
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours


import Prelude hiding ( abs )

demo1 :: IO ()
demo1 = writePS "path1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { setupFont "Times-Roman" 9
                ; ps_translate 60 480 
                ; setRgbColour maroon0
                ; drawLine $ lineS1 (zeroPt::DPoint2)
                ; drawPath $ htildev (P2 0 60)
                }

lineS1 :: DPoint2 -> DLineSegment2
lineS1 = hline 60


htildev :: DPoint2 -> DPath
htildev = \pt -> newPath pt `lineTo` (hline 10) `curveTo` (tildeCurve 40) 
                            `lineTo` (vline 20)


drawPath :: DPath -> WumpusM ()
drawPath = mapM_ fn . unPath where
  fn (Left ln)   = drawLine ln
  fn (Right crv) = drawCurve crv


