{-# OPTIONS -Wall #-}

module Basic1 where

import Wumpus.Core.Wumpus
import Wumpus.Drawing.Basic
import Wumpus.Drawing.PSSkeletons
import Wumpus.Drawing.SVGColours
-- import qualified Wumpus.Drawing.X11Colours as X11

import Wumpus.Core.Point

demo1 :: IO ()
demo1 = writePS "basic1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { translate 60 380 
                ; drawLine $ line (0,0) (0,150)
                ; drawLine $ line (0,0) (150,0)
                ; drawCircle $ circle (5,5) 15
                ; setgray 0.5
                ; drawDisk $ fillColour darkSeaGreen $ disk (60,5) 15
                ; wedge (100,5) 15 0 60
                ; setgray 0 
                ; ellipse (60,5) (15,10)
                ; ellipticarc (120,5) (15,10) 0 270
                ; polygon [(P2 140 5), (P2 140 30), (P2 170 5), (P2 170 30)]
                ; drawPolygon $ diamond (10,50) (30,30)
                ; drawDisk $ disk (10,50) 2
                -- Arrgh -- its now a problem accessing values...
                ; drawPolygon {- (lineColour blueViolet $ lineWidth 1 
                                          $ fillColour burlywood $ envId) -}
                    $ diamond (80,50) (30,20)
                ; setrgbcolor 1 0 0
                ; mapM_ drawLineSegment $ dotPlus  (P2 120 50)
                ; mapM_ drawLineSegment $ dotAsterisk (P2 130 50)
                ; drawPolygon $ dotTriangle        (P2 140 50)
                ; drawPolygon $ dotSquare          (P2 150 50)
                ; drawPolygon $ dotPentagon        (P2 160 50)
                ; mapM_ drawLineSegment $ dotX     (P2 170 50)
                ; drawPolygon $ dotDiamond         (P2 180 50)
                }
