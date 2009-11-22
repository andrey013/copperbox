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
  , rectangle
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

-- | Create a rectangle of width @w@ and height @h@ with the 
-- bottom-left corner located at the supplied point.
--
rectangle :: Num u => u -> u -> Point2 u -> Polygon u
rectangle w h bl = Polygon $ sequence [id,v1,v2,v3] bl where
    v1 = (.+^ hvec w)
    v2 = (.+^ V2 w h)
    v3 = (.+^ vvec h)



-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the supplied point.
regularPolygon :: (Floating u, Real u)
               => Int -> u -> Point2 u -> Polygon u
regularPolygon n r pt = Polygon $ circularAbout pt n r




-- | @isocelesTriangle bw h pt@
--
isoscelesTriangle :: Fractional u => u -> u -> Point2 u -> Polygon u
isoscelesTriangle bw h pt = Polygon [br,top,bl] where
  hh  = h/2
  hw  = bw/2
  top = pt .+^ vvec hh
  br  = pt .+^ V2   hw  (-hh)
  bl  = pt .+^ V2 (-hw) (-hh)


--------------------------------------------------------------------------
-- Drawing polygons

fillPolygon :: (Fill t, Num u, Ord u) => t -> Polygon u -> Primitive u
fillPolygon = appro fill id (vertexPath . vertexList) 

strokePolygon :: (Stroke t, Num u, Ord u) => t -> Polygon u -> Primitive u
strokePolygon = appro cstroke id (vertexPath . vertexList) 

