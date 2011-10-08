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
-- \*\* WARNING \*\* - much of this module is probably obsolete 
-- (except wedge).
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.DrawingPrimitives
  (

  -- * Monoid mappend
    (<>)

  -- * Lines

  , horizontalLine
  , verticalLine
  , pivotLine


  -- * Rectangles
  , blRectangle
  , ctrRectangle


  -- * Wedge
  , wedge

  )

  where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Data.Monoid

infixr 6 <>


-- | Alias for mappend in Monoid.
--
-- >  <> (infixr 6)
-- 
(<>) :: Monoid a => a -> a -> a
(<>) = mappend



--------------------------------------------------------------------------------
-- Lines


-- | Draw a vertical line.
-- 
verticalLine :: InterpretUnit u => u -> LocGraphic u 
verticalLine len = locStraightLine $ vvec len

-- | Draw a horizontal line.
-- 
horizontalLine :: InterpretUnit u => u -> LocGraphic u 
horizontalLine len = locStraightLine $ hvec len



-- | @pivotLine@ : @ left_length * right_length * incline -> LocGraphic @
--
-- Draw a /pivot/ line. The start point is a pivot along the line, 
-- not the end. The left and right distances are the extension of
-- the line from the pivot. 
--
pivotLine :: (Floating u, InterpretUnit u) => u -> u -> Radian -> LocGraphic u
pivotLine lu ru ang = promoteLoc $ \pt -> 
    straightLine (pt .+^ avec (ang+pi) lu) (pt .+^ avec ang ru)




--------------------------------------------------------------------------------
-- Rectangles



-- | Draw a rectangle, start point is bottom left.
--
blRectangle :: InterpretUnit u => DrawMode -> u -> u -> LocGraphic u
blRectangle = dcRectangle


-- | Draw a rectangle, start point is bottom left.
--
ctrRectangle :: (Fractional u, InterpretUnit u) 
             => DrawMode -> u -> u -> LocGraphic u
ctrRectangle mode w h = 
    moveStart (vec (-hw) (-hh)) $ dcRectangle mode w h
  where
    hw = 0.5 * w
    hh = 0.5 * h




--------------------------------------------------------------------------------
-- Wedge



-- | wedge : mode * radius * apex_angle
-- 
-- Wedge is drawn at the apex.
--
wedge :: (Real u, Floating u, InterpretUnit u) 
       => DrawMode -> u -> Radian -> LocThetaGraphic u
wedge mode radius ang = promoteLocTheta $ \pt inclin -> 
    let half_ang = 0.5 * ang 
        line_in  = catline $ avec (inclin + half_ang)   radius
        line_out = catline $ avec (inclin - half_ang) (-radius)
        w_arc    = circularArc CW ang radius (inclin - half_pi)
        ct       = line_in `mappend` w_arc `mappend` line_out
    in supplyLoc pt $ drawCatTrail (closedMode mode) ct
        

