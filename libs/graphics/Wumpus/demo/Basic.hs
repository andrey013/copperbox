{-# OPTIONS -Wall #-}

module Basic1 where


import Wumpus.Core.Line
import Wumpus.Core.Point

import Wumpus.Drawing.Basic
import Wumpus.Drawing.PostScript
import Wumpus.Drawing.PSSkeletons
import Wumpus.Drawing.SVGColours


demo1 :: IO ()
demo1 = writePS "basic1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; drawLine $ lineTo (P2 0 0) (P2 0 150)
                ; drawLine $ lineTo (P2 0 0) (P2 150 0)
                ; drawCircle $ circle (5,5) 15
                ; ps_setgray 0.5
                -- fillColour darkSeaGreen 
                ; drawDisk $  disk (60,5) 15
                ; wedge (100,5) 15 0 60
                ; ps_setgray 0 
                ; ellipse (60,5) (15,10)
                ; ellipticarc (120,5) (15,10) 0 270
                ; polygon [(P2 140 5), (P2 140 30), (P2 170 5), (P2 170 30)]
                ; drawPolygon $ diamond (30,30) (P2 10 50)
                ; drawDisk $ disk (10,50) 2
                -- Properties currently not implemented ...
                -- lineColour blueViolet $ lineWidth 1 $ fillColour burlywood
                ; drawPolygon $ diamond (30,20) (P2 80 50) 
                ; setRgbColour blueViolet
                ; mapM_ drawLineSegment $ dotPlus  (P2 120 50)
                ; mapM_ drawLineSegment $ dotAsterisk (P2 130 50)
                ; drawPolygon $ dotTriangle        (P2 140 50)
                ; drawPolygon $ dotSquare          (P2 150 50)
                ; drawPolygon $ dotPentagon        (P2 160 50)
                ; mapM_ drawLineSegment $ dotX     (P2 170 50)
                ; drawPolygon $ dotDiamond         (P2 180 50)
                }
