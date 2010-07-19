{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.ScatterPlot
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

module Wumpus.PSC.ScatterPlot
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

import Wumpus.PSC.Core
import Wumpus.PSC.DrawingUtils
import Wumpus.PSC.ScaleMonad hiding ( ScaleCtx )

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Control.Monad
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



-- working out...

type DotF' u = GraphicF u


makeLayer' :: (Monad m , CoordScaleM m ux uy u) 
          => (DotF' u,Dataset ux uy) -> m (Graphic u)
makeLayer' (dotF,ds) = mveloH (makeDot' dotF) ds 


makeDot' :: (Monad m , CoordScaleM m ux uy u) 
         => DotF' u -> (ux,uy) -> m (Graphic u)
makeDot' dotF (u,v) = liftM dotF $ coordScale (u,v)




mveloH :: Monad m => (a -> m (H b)) -> [a] -> m (H b)
mveloH mf = step id 
  where
    step acc []     = return acc
    step acc (x:xs) = mf x >>= \a -> step (acc . a) xs
 
