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
                ; drawArrow $ arrow (P2 0 0) (P2 40 40) 
                ; setRgbColour dodgerBlue1 
                ; ps_translate 100 0
                ; arrowhead1 (P2 0 0) (P2 10 50) (arrowheadTriangle 10 (pi/10)) drawPolygon
                ; arrowhead1 (P2 10 0) (P2 20 50) (arrowheadVee 10 (pi/10)) 
                                                  (mapM_ drawLine)
                }
             
arrowhead1 :: DPoint2 -> DPoint2 -> (Double -> DPoint2 -> a) 
                -> (a -> WumpusM()) -> WumpusM ()
arrowhead1 start_pt end_pt arrHead drawFun = 
    do { drawLine $ aline
       ; drawFun $ arrHead theta end_pt
       }
  where
    aline    = lineTo start_pt end_pt
    theta    = pi + (atan $ gradient aline)

