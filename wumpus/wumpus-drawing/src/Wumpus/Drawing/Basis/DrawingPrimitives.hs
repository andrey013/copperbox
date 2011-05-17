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


  -- * Arc and wedge
  , arc
  , wedge

  )

  where

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel

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
pivotLine lu ru ang = promoteLoc $ \pt -> 
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
ctrRectangle sty w h = 
    moveStart (vec (-hw) (-hh)) $ dcRectangle sty w h
  where
    hw = 0.5 * w
    hh = 0.5 * h




--------------------------------------------------------------------------------
-- Wedge


-- | arc : radius * apex_angle
-- 
arc :: (Floating u, InterpretUnit u) => u -> Radian -> LocThetaGraphic u
arc radius ang = promoteLocTheta $ \pt inclin -> 
    let ps = bezierArcPoints ang radius inclin pt
    in zapQuery (curvePP ps) >>= dcOpenPath

-- | wedge : radius * apex_angle
-- 
wedge :: (Floating u, InterpretUnit u) 
      => DrawStyle -> u -> Radian -> LocThetaGraphic u
wedge sty radius ang = promoteLocTheta $ \pt inclin -> 
    let ps = bezierArcPoints ang radius inclin pt
    in uconvertCtxF pt      >>= \dpt -> 
       mapM uconvertCtxF ps >>= \dps -> 
       dcClosedPath sty (build dpt dps)
  where
    -- Note - this relies on an implicit straight line cycle back 
    -- to the start point.
    --
    build :: DPoint2 -> [DPoint2] -> PrimPath
    build pt []         = emptyPrimPath pt
    build pt (p1:ps)    = let cs = curves ps
                          in absPrimPath pt (absLineTo p1 : cs)
    
    curves (a:b:c:ps)   = absCurveTo a b c : curves ps
    curves _            = []
    
