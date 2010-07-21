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

import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad


type DotF u = GraphicF u

type ScatterPlotLayer ux uy u = (DotF u, Dataset ux uy)



-- Return in the ScaleMonad or run it?
--
plotLayers :: (Monad m , CoordScaleM m ux uy u, Real u, Floating u) 
           => [ScatterPlotLayer ux uy u] -> m (Graphic u)
plotLayers xs = mveloH makeLayer xs




makeLayer :: (Monad m , CoordScaleM m ux uy u) 
          => (DotF u, Dataset ux uy) -> m (Graphic u)
makeLayer (dotF,ds) = mveloH (drawAt dotF) ds 

