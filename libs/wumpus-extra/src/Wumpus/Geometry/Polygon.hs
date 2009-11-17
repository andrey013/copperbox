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

module Wumpus.Geometry.Polygon where

import Wumpus.Core
import Wumpus.Geometry.Base

import Data.Aviary
import Data.AffineSpace



newtype Polygon u = Polygon { vertexList :: [Point2 u] }
  deriving (Eq,Show)

type DPolygon = Polygon Double

instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs

 
drawFrame :: (Num u, Ord u) => Picture u -> Picture u
drawFrame p = p `over` frp
  where
    (Frame2 e0 e1 o) = extractFrame p
    xbasis           = vertexPath [o, o .+^ e0]
    ybasis           = vertexPath [o, o .+^ e1]
    frp              = multi $ map frame [zostroke xbasis, zostroke ybasis]

 

fillPolygon :: (Fill t, Num u, Ord u) => t -> Polygon u -> Primitive u
fillPolygon = appro fill id (vertexPath . vertexList) 

strokePolygon :: (Fill t, Num u, Ord u) => t -> Polygon u -> Primitive u
strokePolygon = appro fill id (vertexPath . vertexList) 

{-
-- ARG which combinator is this one? - it's not liftA2
-- its bluebird' with arguments permuted
mab :: (a -> c -> d) -> (b -> c) -> a -> b -> d
mab g f a b = g a (f b)

-- ARG! must work out what functions I need to do this prettily
-}


square :: Num u =>  u -> Point2 u -> Polygon u
square side_length bl = Polygon $ xs where
  xs = sequence [id,f1,f2,f3] bl
  f1 = (.+^ hvec side_length)
  f2 = (.+^ (V2 side_length side_length))
  f3 = (.+^ vvec side_length)


instance ExtractPath (Polygon u) where
  type PathUnit (Polygon u) = u
  extractPath = vertexPath . vertexList


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
