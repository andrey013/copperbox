{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Loop
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Open loop for a circle (useful for automata diagrams).
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Extras.Loop
  ( 
    loop
  , loopPoints
  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Kernel
import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space


-- TODO - Loop is a decoration not a connector.
-- It should probably have the same signature as wedge / arc.

-- | Generate a loop - suitable for decorating a circle.
--
-- The radius and the (implicit) start point are the center and 
-- radius of the initial circle not the loop itself.
--
loop :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
      => u -> Point2 u -> Radian -> Query u (AbsPath u)
loop zradius zctr ang = return $ curvePath $ loopPoints zradius zctr ang



-- Should be able to use trig to get a loop suitable for
-- decorating rectangles (provided the start-to-end arc is smaller
-- than the side length




-- | Note - intermediate names and quadrants represent a loop 
-- drawn upwards.
-- 
loopPoints :: (Real u, Floating u) => u -> Point2 u -> Radian -> [Point2 u]
loopPoints circ_radius circ_ctr incl = 
    [ startl, cp1, cp2, kitel, cp3, cp4, top, cp5, cp6, kiter, cp7, cp8, startr ]
  where
    hw          = 1.25  * circ_radius
    height      = 3.8   * circ_radius
    hminor      = 2.72  * circ_radius
    hbase       = circ_radius / 3
    theta       = toRadian $ asin $ hbase / circ_radius
    start_vec   = avec (circularModulo $ incl - quarter_pi) (0.26 * circ_radius)
    end_vec     = avec (circularModulo $ incl + quarter_pi) (0.26 * circ_radius)    
    minor_down  = negate $ 0.8 * circ_radius 
    major_up    = 0.52 * circ_radius
    top_right   = negate $ 0.8 * circ_radius
    top_left    = 0.8 * circ_radius

    top         = dispParallel height incl circ_ctr
    kiter       = dispOrtho (V2 hminor (-hw)) incl circ_ctr
    kitel       = dispOrtho (V2 hminor (hw) ) incl circ_ctr
    
    startr      = circ_ctr .+^ avec (circularModulo $ incl - theta) circ_radius
    startl      = circ_ctr .+^ avec (circularModulo $ incl + theta) circ_radius

    -- quadrant III
    cp1         = startl .+^ end_vec 
    cp2         = dispParallel minor_down incl kitel

    -- quadrant II 
    cp3         = dispParallel major_up incl kitel
    cp4         = dispPerpendicular top_left incl top

    -- quadrant I
    cp5         = dispPerpendicular top_right incl top
    cp6         = dispParallel major_up incl kiter

    -- quadrant IV 
    cp7         = dispParallel minor_down incl kiter
    cp8         = startr .+^ start_vec
