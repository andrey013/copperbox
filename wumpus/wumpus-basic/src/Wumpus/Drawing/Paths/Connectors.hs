{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Paths.Connectors
-- Copyright   :  (c) Stephen Tetley 2010
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

    ConnectorPath
  , DConnectorPath

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

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Prelude hiding ( length )



type ConnectorPath u = Point2 u -> Point2 u -> Path u

type DConnectorPath = ConnectorPath Double

--------------------------------------------------------------------------------

-- | Connect with a straight line.
--
connLine :: Floating u => ConnectorPath u
connLine = line

-- | Right-angled connector - go vertical, then go horizontal.
--
connRightVH :: Floating u => ConnectorPath u
connRightVH p1@(P2 x1 _) p2@(P2 _ y2) = 
    let mid = P2 x1 y2 in traceLinePoints [p1, mid, p2]

-- | Right-angled connector - go horizontal, then go vertical.
--
connRightHV :: Floating u => ConnectorPath u
connRightHV p1@(P2 _ y1) p2@(P2 x2 _) = 
    let mid = P2 x2 y1 in traceLinePoints [p1, mid, p2]

-- | Right-angled connector - go vertical for the supplied 
-- distance, go horizontal, go vertical again for the 
-- remaining distance.
-- 
connRightVHV :: Floating u => u -> ConnectorPath u
connRightVHV v p1@(P2 x1 _) p2@(P2 x2 _) = traceLinePoints [p1, a1, a2, p2]
  where
    a1 = p1 .+^ vvec v
    a2 = a1 .+^ hvec (x2 - x1)


-- | Right-angled connector - go horizontal for the supplied 
-- distance, go verical, go horizontal again for the 
-- remaining distance.
-- 
connRightHVH :: Floating u => u -> ConnectorPath u
connRightHVH h p1@(P2 _ y1) p2@(P2 _ y2) = traceLinePoints [p1,a1,a2,p2]
  where
    a1 = p1 .+^ hvec h
    a2 = a1 .+^ vvec (y2 - y1)


-- | /Triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connIsosceles :: (Real u, Floating u) => u -> ConnectorPath u 
connIsosceles dy p1 p2 = traceLinePoints [p1, mid_pt, p2]
  where
    mid_pt  = midpointIsosceles dy p1 p2



-- | Double /triangular/ joint.
-- 
-- @u@ is the altitude of the triangle.
--
connIsosceles2 :: (Real u, Floating u) => u -> ConnectorPath u 
connIsosceles2 u p1 p2 = traceLinePoints [ p1, cp1, cp2, p2 ]
  where
    (cp1,cp2) = dblpointIsosceles u p1 p2


-- | /Lightning bolt/ joint - a two joint connector with an /axis/
-- perpendicular to the connector direction.
-- 
-- @u@ is the half length of the of the axis.
--
connLightningBolt :: (Real u, Floating u) => u -> ConnectorPath u 
connLightningBolt u p1 p2 = traceLinePoints [ p1, cp1, cp2, p2 ]
  where
    cp1 = midpointIsosceles   u  p1 p2
    cp2 = midpointIsosceles (-u) p1 p2

--------------------------------------------------------------------------------



-- | Form a curve inside an isosceles triangle. 
--
-- The two Bezier control points take the same point - the
-- altitude of the triangle. The curve tends to be quite shallow
-- relative to the altitude.
--
-- @u@ is the altitude of the triangle.
--
connIsoscelesCurve :: (Real u, Floating u) => u -> ConnectorPath u 
connIsoscelesCurve u p1 p2 = traceCurvePoints [p1, control_pt, control_pt, p2]
  where
    control_pt  = midpointIsosceles u p1 p2
    


-- | Form a curve inside a square. 
--
-- The two Bezier control points take the /top/ corners. The
-- curve tends to be very deep.
-- 
connSquareCurve :: (Real u, Floating u) => ConnectorPath u 
connSquareCurve p1 p2 = traceCurvePoints [p1, cp1, cp2, p2]
  where
    (cp1,cp2) = squareFromBasePoints p1 p2

-- | Form a curve inside a square. 
--
-- As per 'connSquareCurve' but the curve is drawn /underneath/
-- the line formed between the start and end points.
-- 
-- (Underneath is modulo the direction, of course).
-- 
connUSquareCurve :: (Real u, Floating u) => ConnectorPath u 
connUSquareCurve p1 p2 = traceCurvePoints [p1, cp1, cp2, p2]
  where
    (cp1,cp2) = usquareFromBasePoints p1 p2



-- | altitude * ratio_to_base 
--
-- Form a curve inside a trapeziod.
-- 
connTrapezoidCurve :: (Real u, Floating u) => u -> u -> ConnectorPath u 
connTrapezoidCurve u ratio_to_base p1 p2 = traceCurvePoints [p1, cp1, cp2, p2]
  where
    (cp1,cp2)  = trapezoidFromBasePoints u ratio_to_base p1 p2


-- | Make a curve within a square, following the corner points as
-- a Z.
--
connZSquareCurve :: (Real u, Floating u) => ConnectorPath u 
connZSquareCurve p1 p2 = traceCurvePoints [p1,cp1,cp2,p2]
   where
     (cp1,cp2)  = squareFromCornerPoints p1 p2 
      
-- | Make a curve within a square, following the corner points as
-- a Z.
--
-- The order of tracing flips the control points, so this is an
-- /underneath/ version of 'connZSquareCurve'.
-- 
connUZSquareCurve :: (Real u, Floating u) => ConnectorPath u 
connUZSquareCurve p1 p2 = traceCurvePoints [p1,cp2,cp1,p2]
   where
     (cp1,cp2)  = squareFromCornerPoints p1 p2 
