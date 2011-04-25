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

  , isoscelesTrapeziumVertices

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
parallelogramVertices w h bl_ang = (to_bl, to_br, to_tr, to_tl)
  where
    hw              = 0.5 * w
    hh              = 0.5 * h
    hypo            = hh / (fromRadian $ sin bl_ang)

    to_bl           = hvec (-hw) ^+^ avec bl_ang (-hypo)
    to_br           = hvec hw    ^+^ avec bl_ang (-hypo)
    to_tl           = hvec (-hw) ^+^ avec bl_ang hypo
    to_tr           = hvec hw    ^+^ avec bl_ang hypo




-- Trapezium - make an isosceles trapezium...
-- base - top - height 

-- 
isoscelesTrapeziumVertices :: Floating u
                           => u -> u -> u -> Vertices4 u
isoscelesTrapeziumVertices wbase wtop h = 
    (to_bl, to_br, to_tr, to_tl)
  where
    hh    = 0.5 * h
    hbw   = 0.5 * wbase
    htw   = 0.5 * wtop    
    to_bl = V2 (-hbw) (-hh) 
    to_br = V2   hbw  (-hh) 
    to_tl = V2 (-htw)   hh 
    to_tr = V2   htw    hh

