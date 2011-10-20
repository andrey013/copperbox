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

  , ellipse_trail
  , rellipse_trail

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



ellipse_trail :: (Real u, Floating u) => u -> u -> AnaTrail u
ellipse_trail rx ry = rellipse_trail rx ry 0

rellipse_trail :: (Real u, Floating u) => u -> u -> Radian -> AnaTrail u
rellipse_trail rx ry ang = 
    modifyAna (\v -> v ^-^ avec ang rx) $ incline_ellipse ry $ avec ang (2 * rx)


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

{-

--------------------------------------------------------------------------------
-- Re-implementation of Bezier Ellispe from Wumpus-Core 
-- to make an AnaTrail

kappa :: Floating u => u
kappa = 4 * ((sqrt 2 - 1) / 3)


ellipse_trail :: (Real u, Floating u) 
               => u -> u -> AnaTrail u
ellipse_trail rx ry = rellipse_trail rx ry 0


-- | 'rellipse_trail' : @ x_radius * y_radius * center * angle -> AnaTrail @ 
-- 
-- Make an rotated ellipse from four Bezier curves. 
-- 
-- Although this function produces an approximation of a ellipse, 
-- the approximation seems fine in practice.
--
rellipse_trail :: (Real u, Floating u) 
               => u -> u -> Radian -> AnaTrail u
rellipse_trail rx ry ang = 
    anaCatTrail (theta_right rx ang) $ ellipseCat rx ry ang


-- TODO - spilt this to define semiellipse trails and add to 
-- Wumpus-Basic, then it can be used for other things.
--
ellipseCat :: (Real u, Floating u) 
           => u -> u -> Radian -> CatTrail u
ellipseCat rx ry ang = 
       catcurve (pvec p00 c01) (pvec c01 c02) (pvec c02 p03)
    <> catcurve (pvec p03 c04) (pvec c04 c05) (pvec c05 p06) 
    <> catcurve (pvec p06 c07) (pvec c07 c08) (pvec c08 p09) 
    <> catcurve (pvec p09 c10) (pvec c10 c11) (pvec c11 p00)
  where
    lrx   = rx * kappa
    lry   = ry * kappa
    para  = theta_right `flip` ang
    perp  = theta_up `flip` ang

    p00   = zeroPt .+^ theta_right rx ang
    c01   = p00 .+^ perp lry
    c02   = p03 .+^ para lrx

    p03   = zeroPt .+^ theta_up ry ang  
    c04   = p03 .+^ para (-lrx)
    c05   = p06 .+^ perp lry

    p06   = zeroPt .+^ theta_left rx ang
    c07   = p06 .+^ perp (-lry)
    c08   = p09 .+^ para (-lrx)

    p09   = zeroPt .+^ theta_down ry ang
    c10   = p09 .+^ para lrx
    c11   = p00 .+^ perp (-lry)

-}