{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Grids
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

module Wumpus.Drawing.Extras.Grids
  ( 
    grid

  ) where

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative




-- Design note - grid will generally be used as a background so
-- it makes sense to have stroke colour as a parameter - it will 
-- almost always be different to the rest of the drawing.
--


-- | Note - the grid is originated at whatever implicit start
-- point is used. It is not snapped to /nice round/ numbers.
-- 
grid :: (Fractional u, InterpretUnit u)
     => (Int,Int) -> RGBi -> LocGraphic u
grid (nx,ny) rgb    
    | nx < 1 || ny < 1 = emptyLocGraphic
    | otherwise        = local_ctx (stroke_colour rgb) $ 
        snapGridFactors &=> \(x_incr, y_incr) ->
        let rectw  = x_incr * fromIntegral nx
            recth  = y_incr * fromIntegral ny
            grid1  = interiorGrid x_incr nx y_incr ny rectw recth
        in grid1 `oplus` strokedRectangle rectw recth



-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
snapGridFactors :: (Fractional u, InterpretUnit u) =>  Query (u,u)
snapGridFactors = (\(V2 x y) -> (x,y)) <$> snapmove (1,1)


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
interiorGrid :: InterpretUnit u
             => u -> Int -> u -> Int -> u -> u -> LocGraphic u
interiorGrid x_step nx y_step ny w h = hlines `oplus` vlines
  where
    hline1 = locStraightLine (hvec w)
    vline1 = locStraightLine (vvec h)
    vlines = ignoreAns $ moveStart (displaceH x_step) $
               chainH x_step (replicate (nx-1) vline1) 
    hlines = ignoreAns $ moveStart (displaceV y_step) $ 
               chainV y_step (replicate (ny-1) hline1) 

