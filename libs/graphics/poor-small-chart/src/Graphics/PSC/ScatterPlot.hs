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
  -- * Types
    ScatterPlot
  , DotRadius
  , Pt72
  , ScatterPlotConfig(..)
  , ScatterPlotProps(..)

  -- * Write to file
  , writeScatterPlotEPS
  , writeScatterPlotSVG

  -- * Draw
  , drawScatterPlot
  , drawMulti
 
  ) where

import Graphics.PSC.Axis
import Graphics.PSC.Core
import Graphics.PSC.RenderMonad

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative
-- import Control.Monad

type ScatterPlot = DPicture
type DotRadius   = Double
type Pt72        = Int

data ScatterPlotConfig xu yu = ScatterPlotConfig
      { plot_width          :: Pt72
      , plot_height         :: Pt72
      , x_range             :: Range xu
      , y_range             :: Range yu
      }

data ScatterPlotProps = ScatterPlotProps
      { dot_radius         :: DotRadius
      , dot_colour         :: DRGB
      }

type DotData xu yu = (ScatterPlotProps,[(xu,yu)]) 

writeScatterPlotEPS :: FilePath -> ScatterPlot -> IO ()
writeScatterPlotEPS = writeEPS_latin1 


writeScatterPlotSVG :: FilePath -> ScatterPlot -> IO ()
writeScatterPlotSVG = writeSVG_latin1 

type ScatterPlotM u v a = RenderM u v a

run :: ScatterPlotConfig u v -> ScatterPlotM u v a -> (a,DPicture)
run (ScatterPlotConfig {plot_width, plot_height, x_range, y_range}) mf = 
    runRender (makeGeom width height x_range y_range) mf
  where
    width   = fromIntegral plot_width
    height  = fromIntegral plot_height

drawScatterPlot :: ScatterPlotConfig u v -> DotData u v -> ScatterPlot
drawScatterPlot attr dot_data = snd $ run attr $ plotLayer dot_data
                   

-- NOTE constraints on this type sig are too tight...
drawMulti :: ScatterPlotConfig u v 
          -> (AxisLabel u,AxisLabel v) 
          -> [DotData u v] 
          -> ScatterPlot
drawMulti attr (x_axis_lbl, y_axis_lbl) layers = snd $ run attr $ do 
    mapM_ plotLayer layers
    tellList =<< xlabels x_axis_lbl
    tellList =<< ylabels y_axis_lbl


plotLayer :: DotData u v -> ScatterPlotM u v ()
plotLayer (props,points) = 
    tellList =<< plotDots (dot_radius props) (dot_colour props) points
    


plotDots :: DotRadius -> DRGB -> [(u,v)] -> ScatterPlotM u v [DPrimitive] 
plotDots lw rgb pairs = mapM (\pair -> dot lw rgb <$> scalePoint pair) pairs  

dot :: DotRadius -> DRGB -> DPoint2 -> DPrimitive
dot dr rgb pt = ellipse rgb dr dr pt 