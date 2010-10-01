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
    DotF
  , ScatterPlotLayer

  -- * Draw
  , plotLayers

  ) where


import Wumpus.PSC.Bivariate
import Wumpus.PSC.Core ( Dataset )

import Wumpus.Basic.Dots
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

type DotF = DDotLocImage 

type ScatterPlotLayer ux uy = (DotF, Dataset ux uy)


plotLayers :: [ScatterPlotLayer ux uy] -> Bivariate ux uy -> DGraphic
plotLayers xs bv = undefined -- veloH (makeLayer `flip` bv) xs

makeLayer :: (DotF, Dataset ux uy) -> Bivariate ux uy -> DGraphic
makeLayer (dotF,ds) bv = undefined -- veloH (\pt -> dotF (scaleXY pt bv)) ds 
 

