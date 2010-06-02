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

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.Utils

import Wumpus.Core                      -- package: wumpus-core


data ScatterPlot u v = ScatterPlot
      { scatterplot_projs     :: XYProjection u v
      , scatterplot_rect      :: DrawingRectangle
      , scatterplot_grid      :: Maybe (GridConfig u v)
      , scatterplot_legend    :: Maybe ()
      , scatterplot_layers    :: [(LayerConfig, Dataset u v)]
      }


data LayerConfig = LayerConfig
      { dot_colour      :: DRGB
      , dot_radius      :: Double
      }  


-- Fraction constraint is temporary////
renderScatterPlot :: ScatterPlot u v -> Chart
renderScatterPlot (ScatterPlot (px,py) rect mb_grid _legend ls) = 
    concatBackgrounds pic_layers [ grid ]
  where
    pic_layers  = frameMulti $ concat layers

    grid        = fmap (\x -> frameMulti $ drawGrid (fX,fY) x rect) mb_grid

    layers      = map (makeLayer (fX,fY)) ls


    fX          = makeProjector px
    fY          = makeProjector py


makeLayer :: (u -> Double,v -> Double) 
          -> (LayerConfig,Dataset u v) 
          -> [DPrimitive]
makeLayer (fX,fY) (layer,ds) = map (makeDot (fX,fY) layer) ds 


makeDot :: (u -> Double,v -> Double) -> LayerConfig -> (u,v) -> DPrimitive
makeDot (fX,fY) (LayerConfig rgb radius) (u,v) = 
    dot rgb radius (P2 (fX u) (fY v))


dot :: DRGB -> Double -> DPoint2 -> DPrimitive
dot rgb radius pt = ellipse rgb radius radius pt 

