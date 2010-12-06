{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Geometry.Paths
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Paths for /elementary/ shapes - rectangles...
-- 
-- \*\* - WARNING \*\* - half baked. 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Geometry.Paths
  ( 
    rectanglePath
  , diamondPath
  , polygonPoints
  , isoscelesTrianglePath
  , isoscelesTrianglePoints
  , equilateralTrianglePath
  , equilateralTrianglePoints
  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Data.List ( unfoldr )

-- TODO add regular polygon building from old Wumpus-Extra...

-- | Supplied point is /bottom-left/.
--
rectanglePath :: Num u => u -> u -> Point2 u -> PrimPath u
rectanglePath w h bl = primPath bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 



-- | 'diamondPath' : @ half_width * half_height * center_point -> PrimPath @
--
diamondPath :: Num u => u -> u -> Point2 u -> PrimPath u
diamondPath hw hh ctr = primPath s [ lineTo e, lineTo n, lineTo w]
  where
    s     = ctr .+^ vvec (-hh)
    e     = ctr .+^ hvec hw
    n     = ctr .+^ vvec hh
    w     = ctr .+^ hvec (-hw)
    

-- | 'polygonPoints' : @ num_points * radius * center -> [point] @ 
--
polygonPoints :: Floating u => Int -> u -> Point2 u -> [Point2 u]
polygonPoints n radius ctr = unfoldr phi (0,(pi*0.5))
  where
    theta = (pi*2) / fromIntegral n
    
    phi (i,ang) | i < n     = Just (ctr .+^ avec ang radius, (i+1,ang+theta))
                | otherwise = Nothing

-- | @isocelesTriangle bw h pt@
--
-- Supplied point is the centriod of the triangle. This has a 
-- nicer visual balance than using half-height.
--
isoscelesTrianglePoints :: Floating u 
                        => u -> u -> Point2 u -> (Point2 u, Point2 u, Point2 u)
isoscelesTrianglePoints bw h ctr = (bl, br, top) 
  where
    hw         = 0.5*bw 
    theta      = atan $ h / hw
    centroid_h = hw * tan (0.5*theta)
    top        = ctr .+^ vvec (h - centroid_h)
    br         = ctr .+^ V2   hw  (-centroid_h)
    bl         = ctr .+^ V2 (-hw) (-centroid_h)



-- | @isocelesTriangle bw h pt@
--
-- Supplied point is the centriod of the triangle. This has a 
-- nicer visual balance than using half-height.
--
isoscelesTrianglePath :: Floating u => u -> u -> Point2 u -> PrimPath u
isoscelesTrianglePath bw h ctr = primPath bl [ lineTo br, lineTo top ] 
  where
    hw         = 0.5*bw 
    theta      = atan $ h / hw
    centroid_h = hw * tan (0.5*theta)
    top        = ctr .+^ vvec (h - centroid_h)
    br         = ctr .+^ V2   hw  (-centroid_h)
    bl         = ctr .+^ V2 (-hw) (-centroid_h)

equilateralTrianglePoints :: Floating u 
                          => u -> Point2 u -> (Point2 u, Point2 u, Point2 u)
equilateralTrianglePoints sl = isoscelesTrianglePoints sl h
  where
    h = sl * sin (pi/3)

equilateralTrianglePath :: Floating u => u -> Point2 u -> PrimPath u
equilateralTrianglePath sl ctr = primPath bl [ lineTo br, lineTo top ] 
  where
    (bl,br,top) = equilateralTrianglePoints sl ctr