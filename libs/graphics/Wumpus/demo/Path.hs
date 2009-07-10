{-# OPTIONS -Wall #-}

module Path where

import Wumpus.Core.Curve
import Wumpus.Core.Line
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
                ; drawPath $ path htildev (P2 0 60)
                }

lineS1 :: DCoLineSegment2
lineS1 = hline 60


htildev :: H (CoPathSegment Double)
htildev = withLine (hline 10) . withCurve (tildeCurve 40) . withLine (vline 20)

path :: H (CoPathSegment Double) -> DPoint2 -> [PathSegment Double]
path f pt = abs f pt 

drawPath :: [PathSegment Double] -> WumpusM ()
drawPath = mapM_ fn where
  fn (Left ln)   = drawLine ln
  fn (Right crv) = drawCurve crv


