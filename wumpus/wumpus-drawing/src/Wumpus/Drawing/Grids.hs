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

noLine :: Num u => Point2 u -> Graphic u
noLine = openStroke . emptyPath


grid :: Fractional u => (Int,Int) -> u -> LocGraphic u
grid = error "grid - needs a rethink"

{-
grid (nx,ny) incr    
    | nx < 1 || ny < 1 = emptyLocGraphic
    | otherwise        = promoteR1 $ \p0 -> 
        let rw      = incr * fromIntegral nx
            rh      = incr * fromIntegral ny
            xs      = take (nx-1) $ horizontalPoints incr `at` displaceH incr p0
            ys      = take (ny-1) $ verticalPoints   incr `at` displaceV incr p0
            vlines  = map (\pt -> straightLine (vvec rh) `at` pt) xs
            hlines  = map (\pt -> straightLine (hvec rw) `at` pt) ys
        in safeconcat (noLine p0) hlines `oplus` safeconcat (noLine p0) vlines
                                         `oplus` (strokedRectangle rw rh `at` p0)

-}

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
interiorGrid = error "interiorGrid - needs a rethink"
{-
interiorGrid incr = promoteR2 $ \p0 p1 ->
    let xs          = innerHorizontals incr p0 p1
        ys          = innerVerticals   incr p0 p1
        (V2 vx vy)  = pvec p0 p1
        vlines      = map (\pt -> straightLine (vvec vy) `at` pt) xs
        hlines      = map (\pt -> straightLine (hvec vx) `at` pt) ys
    in safeconcat (noLine p0) hlines `oplus` safeconcat (noLine p0) vlines
-}
