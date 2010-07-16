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

import Graphics.PSC.Core
import Graphics.PSC.DrawingUtils

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Data.Maybe


data ScatterPlot u v = ScatterPlot
      { scatterplot_ctx         :: DrawingContext u v
      , scatterplot_labelling   :: LabellingF u v
      }

type DotF = DPoint2 -> DGraphic

type LabellingF u v = ScaleCtx u v DGraphic


dot :: DRGB -> Double -> DotF 
dot rgb radius = disk rgb radius 


outlinedDot :: DRGB -> Double -> DotF 
outlinedDot rgb radius = \pt -> 
    disk (black, LineWidth 0.5) radius pt . disk rgb radius pt

type ScatterPlotLayer u v = (DotF, Dataset u v)


-- Fraction constraint is temporary////
renderScatterPlot :: ScatterPlot u v -> [ScatterPlotLayer u v] -> Chart
renderScatterPlot (ScatterPlot ctx labellingF) ls = 
    fromMaybe errK $ concatBackgrounds pic_layers [ labels ]
  where
    errK        = error "renderScatterPlot - empty Drawing"
    pic_layers  = concatH layers

    labels      :: DGraphic
    labels      = labellingF ctx

    layers      :: [DGraphic]
    layers      = map (\x -> makeLayer x ctx) ls


makeLayer :: (DotF,Dataset u v) -> ScaleCtx u v DGraphic
makeLayer (dotF,ds) = \ctx -> veloH (\pt -> makeDot dotF pt ctx) ds 


makeDot :: DotF -> (u,v) -> ScaleCtx u v DGraphic
makeDot dotF (u,v) = \(_,fX,fY) -> dotF $ P2 (fX u) (fY v)


