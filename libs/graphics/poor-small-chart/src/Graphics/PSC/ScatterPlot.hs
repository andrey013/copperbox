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
  (
  -- * Data types
    ScatterPlot(..)
  , DotConfig(..)

  -- * Draw
  , renderScatterPlot

  )
  where

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core


data ScatterPlot u v = ScatterPlot
      { scatterplot_projs     :: XYProjection u v
      , scatterplot_rect      :: DrawingRectangle
      , scatterplot_grid      :: Maybe (GridConfig u v)
      , scatterplot_axes      :: Maybe (AxisLabelConfig u v)
      , scatterplot_legend    :: Maybe ()
      , scatterplot_layers    :: [(DotConfig, Dataset u v)]
      }


data DotConfig = DotConfig
      { dot_colour      :: DRGB
      , dot_radius      :: Double
      }  


-- Fraction constraint is temporary////
renderScatterPlot :: ScatterPlot u v -> Chart
renderScatterPlot (ScatterPlot (px,py) rect mb_grid mb_axes _legend ls) = 
    concatBackgrounds pic_layers [ grid, axes ]
  where
    pic_layers  = frameMulti $ concat layers

    grid        = fmap (\x -> frameMulti $ drawGrid (fX,fY) x rect) mb_grid

    axes        = fmap (\x -> frameMulti $ drawAxes (fX,fY) x rect) mb_axes

    layers      = map (makeLayer (fX,fY)) ls


    fX          = makeProjector px
    fY          = makeProjector py


makeLayer :: (u -> Double,v -> Double) 
          -> (DotConfig,Dataset u v) 
          -> [DPrimitive]
makeLayer (fX,fY) (dotcfg,ds) = map (makeDot (fX,fY) dotcfg) ds 


makeDot :: (u -> Double,v -> Double) -> DotConfig -> (u,v) -> DPrimitive
makeDot (fX,fY) (DotConfig rgb radius) (u,v) = 
    dot rgb radius (P2 (fX u) (fY v))


dot :: DRGB -> Double -> DPoint2 -> DPrimitive
dot rgb radius pt = ellipse rgb radius radius pt 

