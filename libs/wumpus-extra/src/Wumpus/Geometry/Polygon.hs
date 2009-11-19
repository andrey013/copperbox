{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Polygon
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Polygons...
-- 
--------------------------------------------------------------------------------

module Wumpus.Geometry.Polygon 
  (
  -- * Polygon type
    Polygon
  , DPolygon

  -- * Construction
  , square
  , regularPolygon
  , isoscelesTriangle

  -- * Drawing polygons
  , fillPolygon
  , strokePolygon

  
  ) where

import Wumpus.Core
import Wumpus.Geometry.Base

import Data.Aviary
import Data.AffineSpace



newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double


--------------------------------------------------------------------------------
-- Instances

type instance DUnit (Polygon u) = u

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs


instance ExtractPath (Polygon u) where
  extractPath = vertexPath . vertexList

instance (Num u, Ord u) => Boundary (Polygon u) where
  boundary = trace . vertexList

--------------------------------------------------------------------------------


-- | Create a square of side length @n@ with bottom-left corner 
-- located at the supplied point.
--
square :: Num u => u -> Point2 u -> Polygon u
square w bl = Polygon $ sequence [id,v1,v2,v3] bl where
    v1 = (.+^ hvec w)
    v2 = (.+^ V2 w w)
    v3 = (.+^ vvec w)




-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the supplied point.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Point2 u -> Polygon u
regularPolygon n r pt = Polygon $ circularAbout pt n (pt .+^ V2 r 0)


-- | Create an isosceles rectangle with bottom-left corner at the 
-- origin, the base on the horizontal plane with width @bw@. The 
-- height is @h@.
isoscelesTriangle :: Fractional u => u -> u -> Polygon u
isoscelesTriangle bw h = Polygon $ sequence [id,f2,f3] zeroPt where
  f2 = (.+^ hvec bw)
  f3 = (.+^ V2 (bw/2) h)


--------------------------------------------------------------------------
-- Drawing polygons

fillPolygon :: (Fill t, Num u, Ord u) => t -> Polygon u -> Primitive u
fillPolygon = appro fill id (vertexPath . vertexList) 

strokePolygon :: (Stroke t, Num u, Ord u) => t -> Polygon u -> Primitive u
strokePolygon = appro cstroke id (vertexPath . vertexList) 

