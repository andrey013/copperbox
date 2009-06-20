{-# OPTIONS -Wall #-}

module Arrow1 where

import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.PostScript
import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.X11Colours


demo1 :: IO ()
demo1 = writePS "arrow1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; arrow (P2 0 0) (P2 40 40) 
                ; setRgbColour dodgerBlue1 
                ; ps_translate 100 0
                ; arrowhead1
                }
             
arrowhead1 :: WumpusM ()
arrowhead1 = do { drawLine $ aline
                ; drawPolygon $ arrowheadTriangle 10 (pi/10) theta end_pt
                }
  where
    start_pt = P2 0 0
    end_pt   = P2 10 50
    aline    = lineTo start_pt end_pt
    theta    = pi + (atan $ gradient aline)

dummy = atan $ gradient $ lineTo (P2 0 0) (P2 10 10)  