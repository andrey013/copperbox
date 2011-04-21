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

  , rectangleVertices  
  , isoscelesTriangleVertices
  , equilateralTriangleVertices

  ) 
  where


import Wumpus.Core                              -- package: wumpus-core


type Vertices2 u = (Vec2 u, Vec2 u)
type Vertices3 u = (Vec2 u, Vec2 u, Vec2 u)
type Vertices4 u = (Vec2 u, Vec2 u, Vec2 u, Vec2 u)


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
isoscelesTriangleVertices :: Floating u 
                        => u -> u -> Vertices3 u
isoscelesTriangleVertices bw h = (bl, br, top) 
  where
    hw         = 0.5*bw 
    theta      = atan $ h / hw
    centroid_h = hw * tan (0.5*theta)
    top        = vvec (h - centroid_h)
    br         = V2   hw  (-centroid_h)
    bl         = V2 (-hw) (-centroid_h)



-- | @ side_length -> (BL,BR,Apex)@
--
equilateralTriangleVertices :: Floating u => u -> Vertices3 u
equilateralTriangleVertices sl = isoscelesTriangleVertices sl h
  where
    h = sl * sin (pi/3)

