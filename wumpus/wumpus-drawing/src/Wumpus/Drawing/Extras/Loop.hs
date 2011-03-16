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

-- This could go in the Path namespace?


-- | Note this has problems vis adding tips as the actual start
-- and end points are synthesized.
--
loop :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
     => Query (Point2 u -> Point2 u -> Path u)
loop = promoteQ2 $ \ctr radpt -> 
   let incline = lineDirection ctr radpt
       radius  = abs $ vlength $ pvec ctr radpt
       ps      = loopPoints radius ctr incline
   in return $ traceCurvePoints ps

-- | Note - intermediate names and quadrants represent a loop 
-- drawn upwards.
-- 
loopPoints :: (Real u, Floating u) => u -> Point2 u -> Radian -> [Point2 u]
loopPoints circ_radius circ_ctr incline = 
    [ startl, cp1, cp2, kitel, cp3, cp4, top, cp5, cp6, kiter, cp7, cp8, startr ]
  where
    hw          = 1.25  * circ_radius
    height      = 3.8   * circ_radius
    hminor      = 2.72  * circ_radius
    hbase       = circ_radius / 3
    theta       = toRadian $ asin $ hbase / circ_radius
    start_vec   = avec (circularModulo $ incline - quarter_pi) (0.26 * circ_radius)
    end_vec     = avec (circularModulo $ incline + quarter_pi) (0.26 * circ_radius)    
    minor_down  = negate $ 0.8 * circ_radius 
    major_up    = 0.52 * circ_radius
    top_right   = negate $ 0.8 * circ_radius
    top_left    = 0.8 * circ_radius

    top         = displaceParallel height incline circ_ctr
    kiter       = displaceOrtho (V2 hminor (-hw)) incline circ_ctr
    kitel       = displaceOrtho (V2 hminor (hw) ) incline circ_ctr
    
    startr      = circ_ctr .+^ avec (circularModulo $ incline - theta) circ_radius
    startl      = circ_ctr .+^ avec (circularModulo $ incline + theta) circ_radius

    -- quadrant III
    cp1         = startl .+^ end_vec 
    cp2         = displaceParallel minor_down incline kitel

    -- quadrant II 
    cp3         = displaceParallel major_up incline kitel
    cp4         = displacePerpendicular top_left incline top

    -- quadrant I
    cp5         = displacePerpendicular top_right incline top
    cp6         = displaceParallel major_up incline kiter

    -- quadrant IV 
    cp7         = displaceParallel minor_down incline kiter
    cp8         = startr .+^ start_vec
