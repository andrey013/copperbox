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
  , parallelogramHComponents

  , trapeziumVertices

  ) 
  where


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


parallelogramHComponents :: Fractional u => u -> u -> Radian -> (u,u)
parallelogramHComponents w h bl_ang = (xminor,xmajor)
  where
    half_ang  = 0.5 * bl_ang
    hh        = 0.5 * h
    
    -- | find xminor (adj) from half_angle and hh (op) 
    xminor    = hh / (fromRadian $ tan half_ang)
    xmajor    = w - xminor




trapeziumVertices :: Floating u
                  => u -> u -> Radian -> Radian -> Vertices4 u
trapeziumVertices bw h lang rang = (bl, br, tr, tl)
  where
    half_base = 0.5 * bw
    hh        = 0.5 * h
    br        = V2 half_base (-hh)
    bl        = V2 half_base (-hh)
    tr        = br ^+^ tzRightSideVec h rang
    tl        = bl ^+^ tzLeftSideVec h lang



-- | Calculate the vector that produces the upper-left point given
-- the lower-left point.
--
-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
tzLeftSideVec :: Floating u => u -> Radian -> Vec2 u
tzLeftSideVec h lang | lang <  0.5*pi = less_ninety
                     | lang == 0.5*pi = vvec h
                     | otherwise      = grtr_ninety
  where
    less_ninety = let dist = h / (fromRadian $ sin lang) in avec lang dist
    grtr_ninety = let theta = lang - (0.5*pi) 
                      dist  = h / (fromRadian $ cos theta) 
                  in avec lang dist




-- | Calculate the vector that produces the upper-right point given
-- the lower-right point.
--
-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
tzRightSideVec :: Floating u => u -> Radian -> Vec2 u
tzRightSideVec h rang | rang <  0.5*pi = less_ninety
                      | rang == 0.5*pi = vvec h
                      | otherwise      = grtr_ninety
  where
    less_ninety = let dist  = h / (fromRadian $ sin rang) in avec (pi - rang) dist
    grtr_ninety = let theta = rang - (0.5*pi) 
                      dist  = h / (fromRadian $ cos theta) 
                  in avec (pi - rang) dist

