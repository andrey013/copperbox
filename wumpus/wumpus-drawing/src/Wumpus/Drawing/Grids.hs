{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Grids
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Drawing grids
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Grids
  ( 
    grid
  , interiorGrid

  ) where

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core



grid :: Fractional u => (Int,Int) -> u -> LocGraphic u
grid (nx,ny) incr    
    | nx < 1 || ny < 1 = emptyLocGraphic
    | otherwise        = promoteR1 $ \sw -> 
        let rw      = incr * fromIntegral nx
            rh      = incr * fromIntegral ny
            xchn    = horizontalPoints incr
            ychn    = verticalPoints incr
            vlines  = unchain nx (straightLine (vvec rh)) xchn
            hlines  = unchain ny (straightLine (hvec rw)) ychn
        in (hlines `oplus` vlines `oplus` strokedRectangle rw rh) `at` sw



-- | 'interiorGrid' : @ increment -> ConnectorGraphic @
--
-- Draw the interior lines of a grid between the /connector/ 
-- points - start point is interpreted as bottom-left, end-point
-- is interpreted as top right.
--
-- The interior lines are calculated with repsect to the 0 and the 
-- increment, for instance with an increment of 10 but a start 
-- point @(15,0)@ lines are drawn from @(20,0), (30,0)@ etc.
--
interiorGrid :: RealFrac u => u -> ConnectorGraphic u
interiorGrid incr = promoteR2 $ \sw ne ->
    let xcc        = innerHorizontals incr
        ycc        = innerVerticals   incr
        (V2 vx vy)  = pvec sw ne
        vlines      = unconnectorChain (straightLine (vvec vy)) xcc
        hlines      = unconnectorChain (straightLine (hvec vx)) ycc
    in connect (hlines `oplus` vlines) sw ne

