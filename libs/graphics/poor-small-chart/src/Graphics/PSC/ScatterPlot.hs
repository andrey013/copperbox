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
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )

data ScatterPlot u v = ScatterPlot
      { scatterplot_projs     :: XYProjection u v
      , scatterplot_rect      :: DrawingRectangle
      , scatterplot_grid      :: Maybe (GridConfig u v)
      , scatterplot_axes      :: Maybe (AxisLabelConfig u v)
      , scatterplot_legend    :: Maybe ()
      }

-- TODO - dots could be replcaed with a simple function : Point -> HPrim
data DotConfig = DotConfig
      { dot_colour      :: DRGB
      , dot_radius      :: Double
      , dot_circled     :: Bool
      }  

type ScatterPlotLayer u v = (DotConfig, Dataset u v)


-- Fraction constraint is temporary////
renderScatterPlot :: ScatterPlot u v -> [ScatterPlotLayer u v] -> Chart
renderScatterPlot (ScatterPlot (px,py) rect mb_grid mb_axes _legend) ls = 
    concatBackgrounds pic_layers [ grid, axes ]
  where
    pic_layers  = frameMulti $ toListH $ concatH layers

    grid        :: Maybe DPicture
    grid        = maybe Nothing (\x -> drawGrid (fX,fY) x rect) mb_grid
    
    axes        :: Maybe DPicture
    axes        = maybe Nothing (\x -> drawAxes (fX,fY) x rect) mb_axes

    layers      :: [HPrim Double]
    layers      = map (makeLayer (fX,fY)) ls


    fX          = makeProjector px
    fY          = makeProjector py


makeLayer :: (u -> Double,v -> Double) 
          -> (DotConfig,Dataset u v) 
          -> HPrim Double
makeLayer (fX,fY) (dotcfg,ds) = veloH (makeDot (fX,fY) dotcfg) ds 


makeDot :: (u -> Double,v -> Double) -> DotConfig -> (u,v) -> HPrim Double
makeDot (fX,fY) (DotConfig {dot_colour,dot_radius,dot_circled}) (u,v) =
    if dot_circled then circledDot dot_colour dot_radius pt
                   else        dot dot_colour dot_radius pt
  where
    pt = P2 (fX u) (fY v)


dot :: DRGB -> Double -> DPoint2 -> HPrim Double
dot rgb radius pt = wrapH $ ellipse rgb radius radius pt 

circledDot :: DRGB -> Double -> DPoint2 -> HPrim Double
circledDot rgb radius pt = outline `consH` dot rgb radius pt
  where 
    outline = ellipse (black,LineWidth 0.5) radius radius pt

