{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Polygon
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Polygons...
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Polygon where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.Picture

import Data.AffineSpace



newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs

  
picPolygon :: (Num u, Ord u) => DrawProp -> Polygon u -> Picture u
picPolygon dp (Polygon xs) = Single (ortho zeroPt,trace xs) noProp path
  where 
    path         = Path dp start segs
    (start,segs) = straightLinePath xs



extractPolygonPath :: Polygon u -> Path u
extractPolygonPath p = Path CStroke p0 ps where
  (p0,ps) = straightLinePath $ vertexList p 


bbPolygon :: (Num u, Ord u) => Polygon u -> BoundingBox u
bbPolygon (Polygon xs)     = trace xs 


-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the origin.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Polygon u
regularPolygon n r = Polygon $ circular $ replicate n (zeroPt .+^ (V2 0 r)) 


-- | Create an isosceles rectangle with bottom-left corner at the 
-- origin, the base on the horizontal plane with width @bw@. The 
-- height is @h@.
isoscelesTriangle :: Fractional u => u -> u -> Polygon u
isoscelesTriangle bw h = Polygon $ sequence [id,f2,f3] zeroPt where
  f2 = (.+^ hvec bw)
  f3 = (.+^ V2 (bw/2) h)
