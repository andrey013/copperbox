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

    loopPath
  , loopTrail

  ) where


import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Data.AffineSpace                         -- package: vector-space


import Data.Monoid



loopPath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
      => u -> Point2 u -> Radian -> AbsPath u
loopPath zradius ctr incl = anaTrailPath ctr $ loopTrail zradius incl




-- This is a legacy definition updated to use Trails, hence it is 
-- not so clear.

loopTrail :: (Real u, Floating u) => u -> Radian -> AnaTrail u
loopTrail circ_radius incl = 
    anaCatTrail (pvec zeroPt startl) $ 
       mconcat [ diffCurve startl cp1 cp2 kitel
               , diffCurve kitel  cp3 cp4 top
               , diffCurve top    cp5 cp6 kiter
               , diffCurve kiter  cp7 cp8 startr
               ]
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

    top         = dispParallel height incl zeroPt
    kiter       = dispOrtho hminor (-hw) incl zeroPt
    kitel       = dispOrtho hminor   hw  incl zeroPt
    
    startr      = zeroPt .+^ avec (circularModulo $ incl - theta) circ_radius
    startl      = zeroPt .+^ avec (circularModulo $ incl + theta) circ_radius

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

