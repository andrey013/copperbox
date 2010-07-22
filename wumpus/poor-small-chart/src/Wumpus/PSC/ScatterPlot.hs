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


import Wumpus.PSC.BasicAdditions
import Wumpus.PSC.Core ( Dataset )
import Wumpus.PSC.ScaleRectMonad

import Wumpus.Basic.Graphic                     -- package: wumpus-basic


type DotF = DGraphicF 

type ScatterPlotLayer ux uy = (DotF, Dataset ux uy)



-- Return in the ScaleMonad or run it?
--
plotLayers :: [ScatterPlotLayer ux uy] -> ScaleRectM ux uy DGraphic
plotLayers xs = mveloH makeLayer xs




makeLayer :: (DotF, Dataset ux uy) -> ScaleRectM ux uy DGraphic
makeLayer (dotF,ds) = mveloH (drawAt dotF) ds 

