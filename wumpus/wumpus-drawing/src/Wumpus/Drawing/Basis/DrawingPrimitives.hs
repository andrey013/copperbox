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

  -- * Monoid mappend
    (<>)

  -- * Lines

  , hline
  , vline
  , pivotLine

  , oStraightLines
  , cStraightLines


  -- * Rectangles
  , blRectangle
  , ctrRectangle


  -- * Arc and wedge
  , arc
  , wedge
  , wedge2

  )

  where

import Wumpus.Basic.Geometry                    -- package: wumpus-basic
import Wumpus.Basic.Kernel

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


-- | Draw an open path formed from straight line segments.
--
oStraightLines :: InterpretUnit u => [Point2 u] -> Graphic u
oStraightLines ps = liftQuery (vertexPP ps) >>= dcOpenPath

-- | Draw an closed path formed from straight line segments.
--
cStraightLines :: InterpretUnit u => DrawMode -> [Point2 u] -> Graphic u
cStraightLines mode ps = liftQuery (vertexPP ps) >>= dcClosedPath mode


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


-- | arc : radius * apex_angle
-- 
arc :: (Floating u, InterpretUnit u) => u -> Radian -> LocThetaGraphic u
arc radius ang = promoteLocTheta $ \pt inclin -> 
    let ps = bezierArcPoints ang radius inclin pt
    in liftQuery (curvePP ps) >>= dcOpenPath

-- | wedge : radius * apex_angle
-- 
wedge :: (Floating u, InterpretUnit u) 
      => DrawMode -> u -> Radian -> LocThetaGraphic u
wedge mode radius ang = promoteLocTheta $ \pt inclin -> 
    let ps = bezierArcPoints ang radius inclin pt
    in uconvertCtxF pt      >>= \dpt -> 
       mapM uconvertCtxF ps >>= \dps -> 
       dcClosedPath mode (build dpt dps)
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
    
-- | Wedge is drawn at the apex.
--
wedge2 :: (Real u, Floating u, InterpretUnit u) 
       => DrawMode -> u -> Radian -> LocThetaGraphic u
wedge2 mode radius ang = promoteLocTheta $ \pt inclin -> 
    let half_ang = 0.5 * ang 
        line_in  = catline $ avec (inclin + half_ang)   radius
        line_out = catline $ avec (inclin - half_ang) (-radius)
        w_arc    = circularArcCW ang radius (inclin - half_pi)
        ct       = line_in `mappend` w_arc `mappend` line_out
    in drawCatTrail (closedMode mode) ct `at` pt
        

        -- Note - shouldn\' use a circle sweep, arc /= circle sweep
    