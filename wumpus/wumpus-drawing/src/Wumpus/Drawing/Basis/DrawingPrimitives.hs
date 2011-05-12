{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.DrawingPrimitives
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Alternative to the @DrawingPrimitives@ module in Wumpus-Basic.
-- 
-- The drawing primitives here are either slightly higher level or
-- less general (more quirky).
--
-- This module is expected to be imported qualified - other modules
-- (e.g. shapes and paths) are likely to export conflicting names.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.DrawingPrimitives
  (


  -- * Lines

    hline
  , vline
  , pivotLine

  -- * Rectangles
  , blRectangle
  , ctrRectangle

  )

  where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space



--------------------------------------------------------------------------------
-- Lines

-- | Draw a vertical line.
-- 
vline :: InterpretUnit u => u -> LocGraphic u 
vline len = locStraightLine $ vvec len

-- | Draw a horizontal line.
-- 
hline :: InterpretUnit u => u -> LocGraphic u 
hline len = locStraightLine $ hvec len



-- | @pivotLine@ : @ left_length * right_length * incline -> LocGraphic @
--
-- Draw a /pivot/ line. The start point is a pivot along the line, 
-- not the end. The left and right distances are the extension of
-- the line from the pivot. 
--
pivotLine :: (Floating u, InterpretUnit u) => u -> u -> Radian -> LocGraphic u
pivotLine lu ru ang = promoteR1 $ \pt -> 
    straightLine (pt .+^ avec (ang+pi) lu) (pt .+^ avec ang ru)


--------------------------------------------------------------------------------
-- Rectangles



-- | Draw a rectangle, start point is bottom left.
--
blRectangle :: InterpretUnit u => DrawStyle -> u -> u -> LocGraphic u
blRectangle = dcRectangle


-- | Draw a rectangle, start point is bottom left.
--
ctrRectangle :: (Fractional u, InterpretUnit u) 
             => DrawStyle -> u -> u -> LocGraphic u
ctrRectangle sty w h = moveStart (displace (-hw) (-hh)) $ dcRectangle sty w h
  where
    hw = 0.5 * w
    hh = 0.5 * h
