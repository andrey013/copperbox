{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Geometry.StrictCurve
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- A strict Bezier curve and operations.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Geometry.StrictCurve
  ( 
    StrictCurve(..)
  , bezierLength  
  , subdivide
  , subdividet

  , interCurveLine

  ) 
  where

import Wumpus.Drawing.Geometry.Base

import Wumpus.Core                              -- package: wumpus-core


-- | A Strict cubic Bezier curve.
--
data StrictCurve u = Curve !(Point2 u) !(Point2 u) !(Point2 u) !(Point2 u)
  deriving (Eq,Ord,Show)



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
bezierLength :: (Floating u, Ord u, FromPtSize u)      
            => Point2 u -> Point2 u -> Point2 u -> Point2 u -> u
bezierLength p0 p1 p2 p3 = gravesenLength (fromPtSize 0.1) $ Curve p0 p1 p2 p3


-- | Jens Gravesen\'s bezier arc-length approximation. 
--
-- Note this implementation is parametrized on error tolerance.
--
gravesenLength :: (Floating u, Ord u) => u -> StrictCurve u -> u
gravesenLength err_tol crv = step crv 
  where
    step c = let l1 = ctrlPolyLength c
                 l0 = cordLength c
             in if   l1-l0 > err_tol
                then let (a,b) = subdivide c in step a + step b
                else 0.5*l0 + 0.5*l1

-- | Length of the tree lines spanning the control points.
--
ctrlPolyLength :: Floating u => StrictCurve u -> u
ctrlPolyLength (Curve p0 p1 p2 p3) = len p0 p1 + len p1 p2 + len p2 p3
  where
    len pa pb = vlength $ pvec pa pb


-- | Length of the cord - start point to end point.
--
cordLength :: Floating u => StrictCurve u -> u
cordLength (Curve p0 _ _ p3) = vlength $ pvec p0 p3




-- | Curve subdivision via de Casteljau\'s algorithm.
--
subdivide :: Fractional u 
          => StrictCurve u -> (StrictCurve u, StrictCurve u)
subdivide (Curve p0 p1 p2 p3) =
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
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
           => u -> StrictCurve u -> (StrictCurve u, StrictCurve u)
subdividet t (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = affineComb t p0    p1
    p12   = affineComb t p1    p2
    p23   = affineComb t p2    p3
    p012  = affineComb t p01   p12
    p123  = affineComb t p12   p23
    p0123 = affineComb t p012  p123



--------------------------------------------------------------------------------
-- intersection 

interCurveLine :: (Floating u , Ord u, FromPtSize u)
               => StrictCurve u -> (Point2 u, Point2 u) -> Maybe (Point2 u)
interCurveLine c0 (p,q) = step c0
  where
    eqline  = lineEquation p q
    step c  = case cut c eqline of
                Left pt     -> Just pt      -- cut at start or end
                Right False -> Nothing
                Right True  -> let (a,b) = subdivide c
                               in case step a of
                                   Just pt -> Just pt
                                   Nothing -> step b
 
-- | Is the curve cut by the line? 
--
-- The curve might cut at the start or end points - which is good
-- as it saves performing a subdivision. But make the return type
-- a bit involved.
--
cut :: (Floating u , Ord u, FromPtSize u)
    => StrictCurve u -> LineEquation u -> Either (Point2 u) Bool
cut (Curve p0 p1 p2 p3) line = 
    if d0 `tEQ` 0 then Left p0 else
    if d3 `tEQ` 0 then Left p3 else
    let ds = [d0,d1,d2,d3] in Right $ not $ all pve ds || all nve ds
  where
    tEQ = \a b -> abs (a-b) < tolerance
    pve = (> tolerance)
    nve = (< (negate tolerance))
    d0  = pointLineDistance p0 line 
    d1  = pointLineDistance p1 line 
    d2  = pointLineDistance p2 line 
    d3  = pointLineDistance p3 line 



-- | Note - its important to be tolerant!
--
tolerance :: FromPtSize u => u
tolerance = fromPtSize 0.01
    
