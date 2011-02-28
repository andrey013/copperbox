{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Basic.Paths
-- Copyright   :  (c) Stephen Tetley 2010-2011
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

module Wumpus.Basic.Geometry.Paths
  ( 
    LocCoordPath
  , coordinatePrimPath

  , rectangleCoordPath
  , diamondCoordPath
  , polygonCoordPath
  , isoscelesTriangleCoordPath
  , isoscelesTrianglePoints
  , equilateralTriangleCoordPath
  , equilateralTrianglePoints
  ) 
  where

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space


import Data.List ( unfoldr )


-- | A functional type from /initial point/ to point list.
--
type LocCoordPath u = Point2 u -> [Point2 u]


-- Note - extraction needs a naming scheme - extractFROM or 
-- extractTO? - in either case this might be queuing up 
-- name-clash problems.
--
-- The Path data type will also need a similar function...
--
 
coordinatePrimPath :: PtSize u => Point2 u -> LocCoordPath u -> PrimPath u
coordinatePrimPath pt fn = go (fn pt)
  where
    go ps@(_:_) = vertexPath ps
    go []       = emptyPath pt        -- fallback


-- NOTE - These functions need changing to generate LocCoordPaths...

-- | Supplied point is /bottom-left/, subsequenct points are 
-- counter-clockise so [ bl, br, tr, tl ] .
--
rectangleCoordPath :: Num u => u -> u -> LocCoordPath u
rectangleCoordPath w h bl = [ bl, br, tr, tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 



-- | 'diamondPath' : @ half_width * half_height * center_point -> PrimPath @
--
diamondCoordPath :: Num u => u -> u -> LocCoordPath u
diamondCoordPath hw hh ctr = [ s,e,n,w ]
  where
    s     = ctr .+^ vvec (-hh)
    e     = ctr .+^ hvec hw
    n     = ctr .+^ vvec hh
    w     = ctr .+^ hvec (-hw)
    

-- | 'polygonCoordPath' : @ num_points * radius * center -> [point] @ 
--
polygonCoordPath :: Floating u => Int -> u -> LocCoordPath u
polygonCoordPath n radius ctr = unfoldr phi (0,(pi*0.5))
  where
    theta = (pi*2) / fromIntegral n
    
    phi (i,ang) | i < n     = Just (ctr .+^ avec ang radius, (i+1,ang+theta))
                | otherwise = Nothing



-- | @isocelesTriangle bw h pt@
--
-- Supplied point is the centriod of the triangle. This has a 
-- nicer visual balance than using half-height.
--
isoscelesTriangleCoordPath :: Floating u => u -> u -> LocCoordPath u
isoscelesTriangleCoordPath bw h ctr = [bl,br,top]
  where
    (bl,br,top) = isoscelesTrianglePoints bw h ctr


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


-- | @ side_length * ctr -> [Points] @
--
equilateralTriangleCoordPath :: Floating u => u -> LocCoordPath u
equilateralTriangleCoordPath sl ctr = [bl, br, top] 
  where
    (bl,br,top) = equilateralTrianglePoints sl ctr

equilateralTrianglePoints :: Floating u 
                          => u -> Point2 u -> (Point2 u, Point2 u, Point2 u)
equilateralTrianglePoints sl = isoscelesTrianglePoints sl h
  where
    h = sl * sin (pi/3)

