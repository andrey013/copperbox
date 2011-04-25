{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basic.Vertices
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Vertices generators for elementary objects - triangles.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Geometry.Vertices
  ( 
    
    Vertices2
  , Vertices3
  , Vertices4

  , runVertices2
  , runVertices3
  , runVertices4


  , rectangleVertices  
  , isoscelesTriangleVertices
  , equilateralTriangleVertices

  , parallelogramVertices

  , trapeziumVertices

  , losLegs

  ) 
  where


import Wumpus.Basic.Geometry.Base

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

type Vertices2 u = (Vec2 u, Vec2 u)
type Vertices3 u = (Vec2 u, Vec2 u, Vec2 u)
type Vertices4 u = (Vec2 u, Vec2 u, Vec2 u, Vec2 u)


runVertices2 :: Num u => Point2 u -> Vertices2 u -> [Point2 u]
runVertices2 ctr (v1,v2) = [ctr .+^ v1, ctr .+^ v2]

runVertices3 :: Num u => Point2 u -> Vertices3 u -> [Point2 u]
runVertices3 ctr (v1,v2,v3) = [ctr .+^ v1, ctr .+^ v2, ctr .+^ v3]

runVertices4 :: Num u => Point2 u -> Vertices4 u -> [Point2 u]
runVertices4 ctr (v1,v2,v3,v4) = 
    [ctr .+^ v1, ctr .+^ v2, ctr .+^ v3, ctr .+^ v4]


-- | Vertices are from the center to (bl, br, tr, tl).
--
rectangleVertices :: Num u => u -> u -> Vertices4 u
rectangleVertices hw hh = (bl, br, tr, tl)
  where
    bl = V2 (-hw) (-hh)
    br = V2   hw  (-hh)
    tr = V2   hw    hh
    tl = V2 (-hw)   hh 






-- | @base_width * height -> (BL,BR,Apex)@
--
-- Vertices are from the centeriod to (bl, br,apex).
--

-- | @ height -> (BL,BR,Apex)@
-- 
-- Point is centroid (not incenter).
--
isoscelesTriangleVertices :: Floating u => u -> u -> Vertices3 u
isoscelesTriangleVertices bw h = (bl, br, top) 
  where
    hw            = 0.5*bw 
    centroid_min  = 1 * (h / 3)
    centroid_maj  = h - centroid_min
    top           = vvec centroid_maj
    br            = V2   hw  (-centroid_min)
    bl            = V2 (-hw) (-centroid_min)





-- | @ side_length -> (BL,BR,Apex)@
--
equilateralTriangleVertices :: Floating u => u -> Vertices3 u
equilateralTriangleVertices h = isoscelesTriangleVertices sl h
  where
    sl = 2.0 * (h / tan (pi/3))



parallelogramVertices :: Floating u => u -> u -> Radian -> Vertices4 u
parallelogramVertices w h bl_ang = (bl, br, tr, tl)
  where
    hh              = 0.5 * h
    (xminor,xmajor) = parallelogramHComponents w h bl_ang
    bl              = V2 (-xminor) (-hh)
    br              = V2   xmajor  (-hh)
    tl              = V2 (-xmajor)   hh     -- topleft subtracts major
    tr              = V2   xminor    hh     -- topright adds minor



-- | This is probably wrong and needs more thought.
-- 
-- The concern is for parallelograms that are taller than they 
-- are wide...
--
parallelogramHComponents :: Fractional u => u -> u -> Radian -> (u,u)
parallelogramHComponents bw h bl_ang = (xminor,xmajor)
  where
    half_ang  = 0.5 * bl_ang
    hh        = 0.5 * h
    
    -- | find xminor (adj) from half_angle and hh (op) 
    xminor    = hh / (fromRadian $ tan half_ang)
    xmajor    = bw - xminor



-- | Center is intersection of the diagonals:

trapeziumVertices :: Floating u
                  => u -> u -> Radian -> Radian -> Vertices4 u
trapeziumVertices bw h lang rang = (to_bl, to_br, to_tr, to_tl)
  where
    sine                = fromRadian . sin
    -- WRONG! - diags not necessarily half the ang
    half_lang           = 0.5 * lang    
    half_rang           = 0.5 * rang

    -- find hypotenuses of bottom diagonals to the center.
    (bl_diag,br_diag)   = losLegs half_lang bw half_rang 

    hminor    = bl_diag * sine half_lang 
    hmajor    = h - hminor

    mmratio   = hmajor / hminor

    vscale    = (*^)
    
    -- These are abit convoluted because we have to manufacture
    -- the angle from the intersection center...
    to_bl     = vreverse $ avec half_lang bl_diag
    to_br     = avec (two_pi - half_rang) br_diag

    to_tr     = vscale mmratio $ vreverse to_bl
    to_tl     = vscale mmratio $ vreverse to_br



-- | @law-of-sines legs@ - Find (ab,bc) given ac, bac and bca:
-- 
-- >            b
-- >        . '  \
-- >    . '       \
-- >  a------------c
-- > 
--
-- (Law of sines)
--
losLegs :: Fractional u => Radian -> u -> Radian -> (u,u)
losLegs bAc ac bCa = (ab,bc)
   where
     aBc  = pi - (bAc + bCa)
     sine = fromRadian . sin
     ab   = (ac * sine bCa) / (sine aBc)
     bc   = (ac * sine bAc) / (sine aBc)


