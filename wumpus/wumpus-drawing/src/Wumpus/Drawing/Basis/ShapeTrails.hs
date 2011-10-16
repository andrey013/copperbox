{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basis.ShapeTrails
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Basis.ShapeTrails
  (

    circle_trail
  , rcircle_trail

  , rectangle_trail
  , rrectangle_trail

  , diamond_trail
  , rdiamond_trail

  , isosceles_triangle_trail
  , risosceles_triangle_trail

  , semicircle_trail
  , rsemicircle_trail

  )

  where

import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Basis.InclineTrails

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Data.Monoid


-- Shapes...

circle_trail :: (Real u, Floating u) => u -> AnaTrail u
circle_trail r = rcircle_trail r 0

rcircle_trail :: (Real u, Floating u) => u -> Radian -> AnaTrail u
rcircle_trail r ang = 
    modifyAna (\v -> v ^-^ avec ang r) $ incline_circle $ avec ang (2 * r)


rectangle_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
rectangle_trail w h = rrectangle_trail w h 0


rrectangle_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rrectangle_trail w h ang = 
    anaCatTrail (orthoVec (negate $ 0.5 * w) (negate $ 0.5 * h) ang) catt
  where
    catt = mconcat [ trail_theta_right w ang
                   , trail_theta_up    h ang 
                   , trail_theta_left  w ang 
                   , trail_theta_down  h ang 
                   ]


diamond_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
diamond_trail w h = rdiamond_trail w h 0 

rdiamond_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rdiamond_trail w h ang = 
    anaCatTrail (theta_right hw ang) catt
  where
    hw   = 0.5 * w
    hh   = 0.5 * h
    catt = mconcat [ orthoCatTrail (-hw)   hh  ang
                   , orthoCatTrail (-hw) (-hh) ang 
                   , orthoCatTrail   hw  (-hh) ang 
                   , orthoCatTrail   hw    hh  ang 
                   ]


isosceles_triangle_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
isosceles_triangle_trail bw h = risosceles_triangle_trail bw h 0 

-- | Drawn at the centroid (1/3 * h).
--
risosceles_triangle_trail :: (Real u, Floating u) 
                          => u -> u -> Radian -> AnaTrail u
risosceles_triangle_trail bw h ang = 
    anaCatTrail (orthoVec (negate hbw) (negate $ h / 3) ang) catt
  where
    hbw  = 0.5 * bw
    catt = mconcat [ trail_theta_right bw ang
                   , orthoCatTrail (-hbw)   h  ang
                   , orthoCatTrail (-hbw) (-h) ang
                   ]


semicircle_trail :: (Real u, Floating u) => u -> AnaTrail u
semicircle_trail r = rsemicircle_trail r 0

rsemicircle_trail :: (Real u, Floating u) => u -> Radian -> AnaTrail u
rsemicircle_trail r ang = 
    anaCatTrail (orthoVec (negate r) (negate hminor) ang) catt
  where
    hminor = (4 * r) / (3 * pi)
    catt   =  trail_theta_right (2 * r) ang
           <> semicircleTrail CCW (avec ang (negate $ 2 * r) )