{-# OPTIONS -Wall #-}

module Arrow where

import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Drawing.Arrow
import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.X11Colours


import Wumpus.Core.Radian

demo1 :: IO ()
demo1 = writePS "arrow1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; drawArrow $ arrow (P2 0 0) (P2 40 40) 
                ; setRgbColour dodgerBlue1 
                ; ps_translate 100 0
                ; arrowhead1 (P2 0 0)  (P2 10 50) (arrowheadTriangle 10 (pi/10)) drawPolygon
                ; arrowhead1 (P2 10 0) (P2 20 50) (arrowheadVee 10 (pi/10)) 
                                                  (mapM_ drawLine)
                ; arrowhead1 (P2 20 0) (P2 30 50) (arrowheadPerp 5) (mapM_ drawLine)
                ; mapM_ drawLine $ arrowCenterMarker $ lineTo (P2 30 0) (P2 40 50)
                }
             
arrowhead1 :: DPoint2 -> DPoint2 -> (DRadian -> DPoint2 -> a) 
                -> (a -> WumpusM()) -> WumpusM ()
arrowhead1 start_pt end_pt arrHead drawFun = 
    do { drawLine $ arrline
       ; drawFun $ arrHead theta end_pt
       }
  where
    theta    :: DRadian
    arrline  = lineTo start_pt end_pt
    theta    = pi + (langle arrline)

