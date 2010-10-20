{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Paths.ControlPoints
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Collection of point manufacturing functions.
--
-- \*\* WARNING \*\* this module is experimental and may change 
-- significantly in future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Paths.ControlPoints
  ( 

    midpointIsosceles
  , dblpointIsosceles

  , rectangleFromBasePoints
  , squareFromBasePoints
  , usquareFromBasePoints

  , trapezoidFromBasePoints

  , squareFromCornerPoints

  ) where

import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space




-- | 'midpointIsosceles' : 
-- @ altitude * start_pt * end_pt -> mid_pt @
--
-- Triangular midpoint.
-- 
-- @u@ is the altitude of the triangle - negative values of u 
-- form the triangle below the line.
--
midpointIsosceles :: (Real u, Floating u) 
                  => u -> Point2 u -> Point2 u -> Point2 u
midpointIsosceles u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    mid_pt .+^ avec perp_ang u
  where
    mid_pt    = P2 (x1 + 0.5*(x2-x1)) (y1 + 0.5*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p1 p2) 



-- | 'dblpointIsosceles' : 
-- @ altitude * start_pt * end_pt * (third_pt, two_thirds_pt) @
-- 
-- Double triangular joint - one joint at a third of the line
-- length, the other at two thirds.
-- 
dblpointIsosceles :: (Real u, Floating u) 
                      => u -> Point2 u -> Point2 u -> (Point2 u, Point2 u)  
dblpointIsosceles u p1@(P2 x1 y1) p2@(P2 x2 y2) = 
    (mid1 .+^ avec perp_ang u, mid2 .-^ avec perp_ang u)
  where
    mid1      = P2 (x1 + 0.33*(x2-x1)) (y1 + 0.33*(y2-y1))
    mid2      = P2 (x1 + 0.66*(x2-x1)) (y1 + 0.66*(y2-y1))
    perp_ang  = (pi*0.5) + direction (pvec p1 p2) 


--------------------------------------------------------------------------------



-- | 'rectangleFromBasePoints' : 
-- @ altitude * start_pt * end_pt * (top_left, top_right) @
-- 
-- Control points forming a rectangle. 
--
-- The two manufactured control points form the top corners, 
-- so the supplied points map as @start_point == bottom_left@ and 
-- @end_point == bottom_right@.
--
rectangleFromBasePoints :: (Real u, Floating u) 
                  => u -> Point2 u -> Point2 u -> (Point2 u, Point2 u)
rectangleFromBasePoints u p1 p2 = (cp1, cp2)
  where
    base_vec  = pvec p1 p2
    theta     = direction base_vec
    cp1       = displacePerpendicular u theta p1
    cp2       = displacePerpendicular u theta p2


-- | 'squareFromBasePoints' : 
-- @ start_pt -> end_pt -> (top_left, top_right) @
-- 
-- Control points forming a square - side_len derived from the 
-- distance between start and end points.
--
-- The two manufactured control points form the top corners, 
-- so the supplied points map as @start_point == bottom_left@ and 
-- @end_point == bottom_right@.
--
squareFromBasePoints :: (Real u, Floating u) 
                     => Point2 u -> Point2 u -> (Point2 u, Point2 u)
squareFromBasePoints p1 p2 = rectangleFromBasePoints side_len p1 p2
  where
    side_len  = vlength $ pvec p1 p2


-- | 'usquareFromBasePoints' : 
-- @ start_pt -> end_pt -> (bottom_left, bottom_right) @
-- 
-- Control points forming a square - side_len derived from the 
-- distance between start and end points.
--
-- As per 'squareFromBasePoints' but the square is drawn 
-- /underneath/ the line formed between the start and end points.
-- (Underneath is modulo the direction, of course).
--
-- The two manufactured control points form the /bottom/ corners, 
-- so the supplied points map as @start_point == top_left@ and 
-- @end_point == top_right@.
-- 
usquareFromBasePoints :: (Real u, Floating u) 
                      => Point2 u -> Point2 u -> (Point2 u, Point2 u)
usquareFromBasePoints p1 p2 = rectangleFromBasePoints side_len p1 p2
  where
    side_len  = negate $ vlength $ pvec p1 p2





-- | 'trapezoidFromBasePoints' : 
-- @ altitude * ratio_to_base * start_pt * end_pt -> (top_left, top_right) @
--
-- Control points form an isosceles trapezoid.
--
-- The two manufactured control points form the top corners, 
-- so the supplied points map as @start_point == bottom_left@ and 
-- @end_point == bottom_right@.
-- 
trapezoidFromBasePoints :: (Real u, Floating u) 
                        => u -> u -> Point2 u -> Point2 u 
                        -> (Point2 u, Point2 u) 
trapezoidFromBasePoints u ratio_to_base p1 p2 = (cp1, cp2)
  where
    base_vec  = pvec p1 p2
    base_len  = vlength base_vec
    theta     = direction base_vec
    half_ulen = 0.5 * ratio_to_base * base_len
    base_mid  = displaceParallel (0.5 * base_len) theta p1
    ubase_mid = displacePerpendicular u theta base_mid
    cp1       = displaceParallel (-half_ulen) theta ubase_mid
    cp2       = displaceParallel   half_ulen  theta ubase_mid




-- | 'squareFromCornerPoints' : 
-- @ altitude * start_pt * end_pt * (top_left, bottom_right) @
-- 
-- Control points forming a square bisected by the line from 
-- start_pt to end_pt. 
--
-- The two manufactured control points form the top_left and
-- bottom_right corners, so the supplied points map as 
-- @start_point == bottom_left@ and @end_point == top_right@.
--
squareFromCornerPoints :: (Real u, Floating u) 
                       => Point2 u -> Point2 u -> (Point2 u, Point2 u) 
squareFromCornerPoints p1 p2 = (cp1, cp2)
  where
    base_vec  = pvec p1 p2
    half_len  = 0.5 * (vlength base_vec)
    theta     = direction base_vec
    base_mid  = displaceParallel half_len theta p1
    cp1       = displacePerpendicular   half_len  theta base_mid
    cp2       = displacePerpendicular (-half_len) theta base_mid

