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
import Wumpus.Basic.Utils.HList         -- package: wumpus-basic

import Data.Maybe


data ScatterPlot u v = ScatterPlot
      { scatterplot_ctx         :: DrawingContext u v
      , scatterplot_labelling   :: LabellingF u v
      }

type DotF = DPoint2 -> Graphic

type LabellingF u v = ScaleCtx u v Graphic


dot :: DRGB -> Double -> DotF 
dot rgb radius = filledCircle rgb radius 


outlinedDot :: DRGB -> Double -> DotF 
outlinedDot rgb radius = \pt -> 
    strokedCircle black 0.5 radius pt . filledCircle rgb radius pt

type ScatterPlotLayer u v = (DotF, Dataset u v)


-- Fraction constraint is temporary////
renderScatterPlot :: ScatterPlot u v -> [ScatterPlotLayer u v] -> Chart
renderScatterPlot (ScatterPlot ctx labellingF) ls = 
    fromMaybe errK $ concatBackgrounds pic_layers [ labels ]
  where
    errK        = error "renderScatterPlot - empty Drawing"
    pic_layers  = concatH layers

    labels      :: Graphic
    labels      = labellingF ctx

    layers      :: [Graphic]
    layers      = map (\x -> makeLayer x ctx) ls


makeLayer :: (DotF,Dataset u v) -> ScaleCtx u v Graphic
makeLayer (dotF,ds) = \ctx -> veloH (\pt -> makeDot dotF pt ctx) ds 


makeDot :: DotF -> (u,v) -> ScaleCtx u v Graphic
makeDot dotF (u,v) = \(_,fX,fY) -> dotF $ P2 (fX u) (fY v)


