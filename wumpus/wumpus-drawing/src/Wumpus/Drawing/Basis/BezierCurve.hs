{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.BezierCurve
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This is due to replace the BezierCurve data type in
-- Wumpus.Basic.Geometry...
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.BezierCurve
  ( 

    BezierCurve(..)
  , vbezierCurve
  , subdivide
  , subdividet

  , bezierLength

  ) where

import Wumpus.Drawing.Basis.Geometry


import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


--------------------------------------------------------------------------------



-- | A Strict cubic Bezier curve.
--
data BezierCurve u = BezierCurve !(Point2 u) !(Point2 u) !(Point2 u) !(Point2 u)
  deriving (Eq,Ord,Show)

type instance DUnit (BezierCurve u) = u



vbezierCurve :: Num u 
             => Vec2 u -> Vec2 u -> Vec2 u -> Point2 u -> BezierCurve u
vbezierCurve v1 v2 v3 p0 = BezierCurve p0 p1 p2 p3
  where
    p1 = p0 .+^ v1
    p2 = p1 .+^ v2
    p3 = p2 .+^ v3



-- | Curve subdivision via de Casteljau\'s algorithm.
--
subdivide :: Fractional u 
          => BezierCurve u -> (BezierCurve u, BezierCurve u)
subdivide (BezierCurve p0 p1 p2 p3) =
    (BezierCurve p0 p01 p012 p0123, BezierCurve p0123 p123 p23 p3)
  where
    p01   = midpoint p0    p1
    p12   = midpoint p1    p2
    p23   = midpoint p2    p3
    p012  = midpoint p01   p12
    p123  = midpoint p12   p23
    p0123 = midpoint p012  p123


-- | subdivide with an affine weight along the line...
--
subdividet :: Real u
           => u -> BezierCurve u -> (BezierCurve u, BezierCurve u)
subdividet t (BezierCurve p0 p1 p2 p3) = 
    (BezierCurve p0 p01 p012 p0123, BezierCurve p0123 p123 p23 p3)
  where
    p01   = affineComb t p0    p1
    p12   = affineComb t p1    p2
    p23   = affineComb t p2    p3
    p012  = affineComb t p01   p12
    p123  = affineComb t p12   p23
    p0123 = affineComb t p012  p123


--------------------------------------------------------------------------------

-- | 'bezierLength' : @ start_point * control_1 * control_2 * 
--        end_point -> Length @ 
--
-- Find the length of a Bezier curve. The result is an 
-- approximation, with the /tolerance/ is 0.1 of a point. This
-- seems good enough for drawing (potentially the tolerance could 
-- be larger still). 
--
-- The result is found through repeated subdivision so the 
-- calculation is potentially costly.
--
bezierLength :: (Floating u, Ord u, Tolerance u)
             => BezierCurve u -> u
bezierLength = gravesenLength length_tolerance 



-- | Jens Gravesen\'s bezier arc-length approximation. 
--
-- Note this implementation is parametrized on error tolerance.
--
gravesenLength :: (Floating u, Ord u) => u -> BezierCurve u -> u
gravesenLength err_tol crv = step crv 
  where
    step c = let l1 = ctrlPolyLength c
                 l0 = cordLength c
             in if   l1-l0 > err_tol
                then let (a,b) = subdivide c in step a + step b
                else 0.5*l0 + 0.5*l1

-- | Length of the tree lines spanning the control points.
--
ctrlPolyLength :: Floating u => BezierCurve u -> u
ctrlPolyLength (BezierCurve p0 p1 p2 p3) = len p0 p1 + len p1 p2 + len p2 p3
  where
    len pa pb = vlength $ pvec pa pb


-- | Length of the cord - start point to end point.
--
cordLength :: Floating u => BezierCurve u -> u
cordLength (BezierCurve p0 _ _ p3) = vlength $ pvec p0 p3


