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
  , interiorGrid

  ) where

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
snapGridFactors :: (Fractional u, FromPtSize u, DrawingCtxM m) 
                =>  m (u,u)
snapGridFactors = 
    bimap conv conv <$> query dc_snap_grid_factors
  where
    conv x = (realToFrac x) * fromPtSize 1


-- Design note - grid will generally be used as a background so
-- it makes sense to have stroke colour as a parameter - it will 
-- almost always be different to the rest of the drawing.
--


-- | Note - the grid is originated at whatever implicit start
-- point is used. It is not snapped to /nice round/ numbers.
-- 
grid :: (Fractional u, FromPtSize u) 
     => (Int,Int) -> RGBi -> LocGraphic u
grid (nx,ny) rgb    
    | nx < 1 || ny < 1 = emptyLocGraphic
    | otherwise        = localize (stroke_colour rgb) $ promoteR1 $ \sw -> 
        snapGridFactors >>= \(x_incr,y_incr) ->
        let rectw  = x_incr * fromIntegral nx
            recth  = y_incr * fromIntegral ny
            xchn    = horizontalPoints x_incr
            ychn    = verticalPoints   y_incr
            vline1  = straightLine (vvec recth)
            hline1  = straightLine (hvec rectw)
            vlines  = unchain nx emptyLocGraphic vline1 xchn
            hlines  = unchain ny emptyLocGraphic hline1 ychn
        in (hlines `oplus` vlines `oplus` strokedRectangle rectw recth) `at` sw



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
interiorGrid  incr = promoteR2 $ \sw ne ->
    let xcc         = innerHorizontals incr
        ycc         = innerVerticals   incr
        (V2 vx vy)  = pvec sw ne
        hline1      = straightLine (hvec vx)
        vline1      = straightLine (vvec vy)
        vlines      = unconnectorChain emptyConnectorGraphic vline1 xcc
        hlines      = unconnectorChain emptyConnectorGraphic hline1 ycc
    in connect (hlines `oplus` vlines) sw ne

-- Design note - at the moment it seems fine that @interiorGrid@ 
-- does not use the snapping grid factors.
-- 
-- However this might be reconsidered.
-- 
