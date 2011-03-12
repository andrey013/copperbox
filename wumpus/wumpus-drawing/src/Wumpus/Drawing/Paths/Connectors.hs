{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Connectors
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Library of connector paths...
--
-- \*\* WARNING \*\* this module is experimental and may change 
-- significantly in future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Paths.Connectors
  ( 

    PathQuery
  , DPathQuery
  , sconnect

  , connLine

  , connRightVH
  , connRightHV
  , connRightVHV
  , connRightHVH

  , connIsosceles
  , connIsosceles2
  , connLightningBolt

  , connIsoscelesCurve
  , connSquareCurve
  , connUSquareCurve

  , connTrapezoidCurve
  , connZSquareCurve
  , connUZSquareCurve

  ) where

import Wumpus.Drawing.Paths.Base
import Wumpus.Drawing.Paths.ControlPoints

import Wumpus.Basic.Geometry.Base               -- package: wumpus-basic
import Wumpus.Basic.Kernel
import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Prelude hiding ( length )


-- | Note - a PathCF is a context function not a graphic 
--  

type PathQuery u = Query ( Point2 u -> Point2 u -> Path u )

type DPathQuery  = PathQuery Double

promoteQuery :: (Point2 u -> Point2 u -> Query (Path u)) -> PathQuery u
promoteQuery fn = makeQuery id $ \ctx p0 p1 -> runQuery (fn p0 p1) ctx

cfPathQuery :: (Point2 u -> Point2 u -> Path u) -> PathQuery u
cfPathQuery fn = makeQuery id $ \_ -> fn

-- pathQuery :: 

-- Maybe this should be PathCF u -> ConnectorImage u (Path u) instead?
-- This would be closer to the new shapes...
--
sconnect :: InterpretUnit u 
         => PathQuery u -> Point2 u -> Point2 u -> Image Path u
sconnect mf p0 p1 = 
    bindQuery_i mf $ \fn -> 
      let cpath = fn p0 p1 
      in bindQuery_i (toPrimPath cpath) $ \ppath -> 
         intoImage (pure cpath) (openStroke ppath)
                    

-- | Build the path with interior round corners.
-- 
roundCornerPath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
                => [Point2 u] -> Query (Path u)
roundCornerPath xs = info roundCornerSize >>= \sz -> 
    if sz == 0 then return (traceLinePoints xs) 
               else return (roundInterior  sz xs)



--------------------------------------------------------------------------------

-- | Connect with a straight line.
--
connLine :: Floating u => PathQuery u
connLine = cfPathQuery $ \p0 p1 -> line p0 p1



-- | Right-angled connector - go vertical, then go horizontal.
--
connRightVH :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
            => PathQuery u
connRightVH = promoteQuery $ \ p0@(P2 x0 _) p1@(P2 _ y1) ->
    let mid = P2 x0 y1 in roundCornerPath [p0, mid, p1]



-- | Right-angled connector - go horizontal, then go vertical.
--
connRightHV :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
            => PathQuery u
connRightHV = promoteQuery $ \ p0@(P2 _ y0) p1@(P2 x1 _) -> 
    let mid = P2 x1 y0 in roundCornerPath [p0, mid, p1]

-- | Right-angled connector - go vertical for the supplied 
-- distance, go horizontal, go vertical again for the 
-- remaining distance.
-- 
connRightVHV :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
             => u -> PathQuery u
connRightVHV v = promoteQuery $ \ p0@(P2 x0 _) p1@(P2 x1 _) ->
    let a0 = p0 .+^ vvec v
        a1 = a0 .+^ hvec (x1 - x0)
    in roundCornerPath [p0, a0, a1, p1]


-- | Right-angled connector - go horizontal for the supplied 
-- distance, go verical, go horizontal again for the 
-- remaining distance.
-- 
connRightHVH :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
             => u -> PathQuery u
connRightHVH h = promoteQuery $ \ p0@(P2 _ y0) p1@(P2 _ y1) -> 
    let a0 = p0 .+^ hvec h
        a1 = a0 .+^ vvec (y1 - y0)
    in roundCornerPath [p0,a0,a1,p1]


-- | /Triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connIsosceles :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
              => u -> PathQuery u 
connIsosceles dy = promoteQuery $ \ p0 p1 -> 
    let mid_pt  = midpointIsosceles dy p0 p1
    in roundCornerPath [p0, mid_pt, p1]
 
    



-- | Double /triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connIsosceles2 :: (Real u, Floating u, InterpretUnit u, LengthTolerance u)
               => u -> PathQuery u 
connIsosceles2 u = promoteQuery $ \ p0 p1 -> 
    let (cp0,cp1) = dblpointIsosceles u p0 p1
    in roundCornerPath [ p0, cp0, cp1, p1 ]



-- | /Lightning bolt/ joint - a two joint connector with an /axis/
-- perpendicular to the connector direction.
-- 
-- @u@ is the half length of the of the axis.
--
connLightningBolt :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
                  => u -> PathQuery u 
connLightningBolt u = promoteQuery $ \ p0 p1 -> 
    let cp0 = midpointIsosceles   u  p0 p1
        cp1 = midpointIsosceles (-u) p0 p1
    in roundCornerPath [ p0, cp0, cp1, p1 ]

--------------------------------------------------------------------------------



-- | Form a curve inside an isosceles triangle. 
--
-- The two Bezier control points take the same point - the
-- altitude of the triangle. The curve tends to be quite shallow
-- relative to the altitude.
--
-- @u@ is the altitude of the triangle.
--
connIsoscelesCurve :: (Real u, Floating u, LengthTolerance u) 
                   => u -> PathQuery u 
connIsoscelesCurve u = promoteQuery $ \ p0 p1 ->
    let control_pt  = midpointIsosceles u p0 p1
    in pure $ traceCurvePoints [p0, control_pt, control_pt, p1]
   
    

-- | Form a curve inside a square. 
--
-- The two Bezier control points take the /top/ corners. The
-- curve tends to be very deep.
-- 
connSquareCurve :: (Real u, Floating u, LengthTolerance u) 
                => PathQuery u 
connSquareCurve = promoteQuery $ \ p0 p1 ->
    let (cp0,cp1) = squareFromBasePoints p0 p1
    in pure $ traceCurvePoints [p0, cp0, cp1, p1]



-- | Form a curve inside a square. 
--
-- As per 'connSquareCurve' but the curve is drawn /underneath/
-- the line formed between the start and end points.
-- 
-- (Underneath is modulo the direction, of course).
-- 
connUSquareCurve :: (Real u, Floating u, LengthTolerance u) 
                 => PathQuery u 
connUSquareCurve = promoteQuery $ \ p0 p1 -> 
    let (cp0,cp1) = usquareFromBasePoints p0 p1
    in pure $ traceCurvePoints [p0, cp0, cp1, p1]


-- | altitude * ratio_to_base 
--
-- Form a curve inside a trapeziod.
-- 
connTrapezoidCurve :: (Real u, Floating u, LengthTolerance u) 
                   => u -> u -> PathQuery u 
connTrapezoidCurve u ratio_to_base = promoteQuery $ \p0 p1 -> 
    let (cp0,cp1)  = trapezoidFromBasePoints u ratio_to_base p0 p1
    in pure $ traceCurvePoints [p0, cp0, cp1, p1]


-- | Make a curve within a square, following the corner points as
-- a Z.
--
connZSquareCurve :: (Real u, Floating u, LengthTolerance u) 
                 => PathQuery u 
connZSquareCurve = promoteQuery $ \p0 p1 -> 
    let (cp0,cp1)  = squareFromCornerPoints p0 p1
    in pure $ traceCurvePoints [p0,cp0,cp1,p1]

-- | Make a curve within a square, following the corner points as
-- a Z.
--
-- The order of tracing flips the control points, so this is an
-- /underneath/ version of 'connZSquareCurve'.
-- 
connUZSquareCurve :: (Real u, Floating u, LengthTolerance u) 
                  => PathQuery u 
connUZSquareCurve = promoteQuery $ \ p0 p1 ->  
   let (cp0,cp1)  = squareFromCornerPoints p0 p1 
   in pure $ traceCurvePoints [p0,cp1,cp0,p1]

