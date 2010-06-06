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
  , DotF

  -- * Draw
  , dot
  , outlinedDot
  , renderScatterPlot

  )
  where

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Data.Maybe


data ScatterPlot u v = ScatterPlot
      { scatterplot_projs     :: XYProjection u v
      , scatterplot_rect      :: DrawingRectangle
      , scatterplot_grid      :: Maybe (GridConfig u v)
      , scatterplot_axes      :: Maybe (AxisLabelConfig u v)
      , scatterplot_legend    :: Maybe ()
      }

type DotF = DPoint2 -> Graphic


dot :: DRGB -> Double -> DotF 
dot rgb radius = filledCircle rgb radius 


outlinedDot :: DRGB -> Double -> DotF 
outlinedDot rgb radius = \pt -> 
    strokedCircle black 0.5 radius pt . filledCircle rgb radius pt

type ScatterPlotLayer u v = (DotF, Dataset u v)


-- Fraction constraint is temporary////
renderScatterPlot :: ScatterPlot u v -> [ScatterPlotLayer u v] -> Chart
renderScatterPlot (ScatterPlot (px,py) rect mb_grid mb_axes _legend) ls = 
    fromMaybe errK $ concatBackgrounds pic_layers [ grid, axes ]
  where
    errK        = error "renderScatterPlot - empty Drawing"
    pic_layers  = concatH layers

    grid        :: Graphic
    grid        = maybe id (\x -> drawGrid (fX,fY) x rect) mb_grid
    
    axes        :: Graphic
    axes        = maybe id (\(AxisLabelConfig x y) -> 
                             drawAxes x y (fX,fY) rect) mb_axes

    layers      :: [Graphic]
    layers      = map (makeLayer (fX,fY)) ls


    fX          = makeProjector px
    fY          = makeProjector py


makeLayer :: (u -> Double,v -> Double) 
          -> (DotF,Dataset u v) 
          -> Graphic
makeLayer (fX,fY) (dotF,ds) = veloH (makeDot (fX,fY) dotF) ds 


makeDot :: (u -> Double,v -> Double) -> DotF -> (u,v) -> Graphic
makeDot (fX,fY) dotF (u,v) = dotF $ P2 (fX u) (fY v)


