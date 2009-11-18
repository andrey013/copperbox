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

square :: Num u =>  u -> Point2 u -> Polygon u
square side_length bl = Polygon $ xs where
  xs = sequence [id,f1,f2,f3] bl
  f1 = (.+^ hvec side_length)
  f2 = (.+^ (V2 side_length side_length))
  f3 = (.+^ vvec side_length)




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


--------------------------------------------------------------------------
-- Drawing polygons

fillPolygon :: (Fill t, Num u, Ord u) => t -> Polygon u -> Primitive u
fillPolygon = appro fill id (vertexPath . vertexList) 

strokePolygon :: (Stroke t, Num u, Ord u) => t -> Polygon u -> Primitive u
strokePolygon = appro cstroke id (vertexPath . vertexList) 

