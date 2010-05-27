{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.ScatterPlot
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Scatter plots...
-- 
--------------------------------------------------------------------------------

module Graphics.PSC.ScatterPlot
 where

import Graphics.PSC.Core

import Wumpus.Core                      -- package: wumpus-core

type ScatterPlot = DPicture
type Pt72 = Int

data ScatterPlotConfig xu yu = ScatterPlotConfig
      { plot_width          :: Pt72
      , plot_height         :: Pt72
      , x_range             :: xu -> Double
      , y_range             :: yu -> Double
      }



data Geom xu yu = Geom 
      { rect_height         :: Double
      , rect_width          :: Double
      , rescale_x           :: xu -> Double
      , rescale_y           :: yu -> Double
      }


{-

makeGeom :: ScatterPlotConfig xu yu -> Geom xu yu
makeGeom attr@(ScatterPlotConfig {point_size,word_length})  = 
    Geom { rect_width   = fromIntegral plot_width
         , rect_height  = fromIntegral plot_height 
         , rescale_x    = makeRescaleX attr
         , rescale_y    = makeRescaleY attr
         }
-}



writeScatterPlotEPS :: FilePath -> ScatterPlot -> IO ()
writeScatterPlotEPS = writeEPS_latin1 


writeScatterPlotSVG :: FilePath -> ScatterPlot -> IO ()
writeScatterPlotSVG = writeSVG_latin1 


drawScatterPlot :: DRGB -> [(Double,Double)] -> ScatterPlot
drawScatterPlot rgb = frameMulti . plot rgb

plot :: DRGB -> [(Double,Double)] -> [DPrimitive]
plot rgb = map fn where
    fn (x,y) = ellipse rgb 2 2 (P2 x y)



dot :: LineWidth -> DRGB -> DPoint2 -> DPrimitive
dot lw rgb pt = ellipse rgb (2*lw) (3*lw) pt 