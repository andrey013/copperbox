{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Polygon
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Polygon
--
--------------------------------------------------------------------------------


module Wumpus.Core.Polygon
  (
  -- * Polygon types
    Polygon(..)
  , DPolygon

  -- * Construction
  , regularPolygon
  , square
  , unitSquare

  , rectangle
  , isoscelesTriangle

  -- * Predicates
  , simplePolygon
  , concavePolygon

  -- * Operations
  , interiorAngles
  , simpleOrientation
  , reverseOrientation  
  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Geometric
import Wumpus.Core.Instances ()
import Wumpus.Core.Matrix
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( nub )

--------------------------------------------------------------------------------
-- Polygon types and standard instances


newtype Polygon a = Polygon { getPolygonPoints :: [Point2 a] }
  deriving (Eq,Show)


type DPolygon = Polygon Double



instance Pointwise (Polygon a) where
  type Pt (Polygon a) = Point2 a
  pointwise f (Polygon xs) = Polygon $ map f xs


instance HasPoints (Polygon a) where
  type Pnt (Polygon a) = Point2 a
  extractPoints (Polygon xs) = xs
  endPoint (Polygon (x:_))   = x        -- start point is also end point   
  endPoint (Polygon _)       = error "endPoint: malformed Polygon, too few points"
  startPoint                 = endPoint  

                
--------------------------------------------------------------------------------
-- Construction


-- | Create a regular polygon with @n@ sides and /radius/ @r@.
regularPolygon :: (Floating a, Real a, AffineSpace a)
               => Int -> a -> (Point2 a -> Polygon a)
regularPolygon n r = Polygon . pf
  where 
    pf = \(P2 x y) -> map (translate x y) 
                          $ circular 
                          $ replicate n (zeroPt .+^ (V2 0 r)) 


-- Note square and rectangle are both 'turtle drawn' and use the @iter@ 
-- functional to successively transform the current point.

-- | Create a square with bottom-left corner @p@ and side-length @d@.
square :: (Num a, AffineSpace a) => a -> (Point2 a -> Polygon a)
square d = Polygon . iter [id,f2,f3,f4] where
  f2 = (.+^ hvec d)
  f3 = (.+^ vvec d)
  f4 = (.+^ (hvec $ negate d))
  
unitSquare :: (Num a, AffineSpace a) => Point2 a -> Polygon a
unitSquare = square 1 


-- | Create a rectangle with bottom-left corner @p@ and width @w@ and
-- height @h@.
rectangle :: (Num a, AffineSpace a) => a -> a -> (Point2 a -> Polygon a)
rectangle w h = Polygon . iter [id,f2,f3,f4] where
  f2 = (.+^ hvec w)
  f3 = (.+^ vvec h)
  f4 = (.+^ (hvec $ negate w))


-- | Create an isosceles rectangle with bottom-left corner @p@, the base 
-- in on the horizontal plane with width @bw@. Height is @h@.
isoscelesTriangle :: (Fractional a, AffineSpace a) 
                  => a -> a -> (Point2 a -> Polygon a)
isoscelesTriangle bw h = Polygon . sequence [id,f2,f3] where
  f2 = (.+^ hvec bw)
  f3 = (.+^ V2 (bw/2) h)




--------------------------------------------------------------------------------
-- predicates

-- | This definition is not satisfactory...
simplePolygon :: Eq a => Polygon a -> Bool
simplePolygon (Polygon ps) 
   | length ps >= 2 = total_len == length (nub ps)
   | otherwise      = error $ "simplePolygon: malformed too few points"
  where
    total_len       = (length ps) - consecutive_pts
    consecutive_pts = windowedFoldR2c fn 0 ps
    fn p p' n       | p==p'     = n+1
                    | otherwise = n
             

-- | A polygon is concave if at least 1 interior angle is greater then pi/2.
-- concavePolygon :: (Ord a, Floating a) => Polygon a -> Bool
concavePolygon :: (Ord a, Floating a, Real a, 
                   AffineSpace a, InnerSpace a, a ~ Scalar a)
               => Polygon a -> Bool
concavePolygon = any (>pi/2) . interiorAngles



--------------------------------------------------------------------------------
-- Operations

-- | Extract the interior angles of polygon.
interiorAngles :: (a ~ Scalar a, Floating a, Real a, 
                   AffineSpace a, InnerSpace a)
               => Polygon a -> [Radian]
interiorAngles (Polygon ps) = windowedMap3c intAng ps where
  intAng a b c = interiorAngle (a .-. b) (c .-. b)

-- | Simple algorithm to deduce orientation of a polygon.
-- Will throw an error if it cannot find two successive vertices 
-- that are not collinear.
simpleOrientation :: Real a => Polygon a -> Orientation
simpleOrientation (Polygon ps) = sign $ det $ mkMat $ noncollinear ps
  where
    noncollinear (x:y:z:xs) 
       | not (collinear x y z)  = (x,y,z)
       | otherwise              = noncollinear (y:z:xs)
    noncollinear _              = error $ "Polygon.simpleOrientation - "
                                       ++ "could not find viable vertices."

    mkMat (P2 x1 y1, P2 x2 y2, P2 x3 y3) = M3'3 1 x1 y1 1 x2 y2 1 x3 y3

    sign a | a < 0     = CW
           | a > 0     = CCW
           | otherwise = error "Polygon.simpleOrientation - det=0"


-- | Reverse the orientation of the polygon (i.e. reverse 
-- the list of points that represent the polygon).
reverseOrientation :: Polygon a -> Polygon a
reverseOrientation = Polygon . reverse . getPolygonPoints