{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import Wumpus.Core.Transform
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( nub )

--------------------------------------------------------------------------------
-- Polygon types and standard instances


newtype Polygon pt = Polygon { getPolygonPoints :: [pt] }
  deriving (Eq,Show)


type DPolygon = Polygon (Point2 Double)



instance Pointwise (Polygon pt) where
  type Pt (Polygon pt) = pt
  pointwise f (Polygon xs) = Polygon $ map f xs


instance HasPoints (Polygon pt) where
  type Pnt (Polygon pt) = pt
  extractPoints (Polygon xs) = xs
  endPoint (Polygon (x:_))   = x        -- start point is also end point   
  endPoint (Polygon _)       = error "endPoint: malformed Polygon, too few points"
  startPoint                 = endPoint  


instance MatrixMult Matrix3'3 pt => MatrixMult Matrix3'3 (Polygon pt) where
  type MatrixParam (Polygon pt) = MatrixParam pt
  (*#) m3'3 (Polygon ps) = Polygon (map (m3'3 *#) ps)


                
--------------------------------------------------------------------------------
-- Construction


-- | Create a regular polygon with @n@ sides and /radius/ @r@ 
-- centered at the origin.
regularPolygon :: (Floating a, Real a, ZeroPt pt, MatrixMult Matrix3'3 pt,
                   AffineSpace pt, Diff pt ~ Vec2 a,
                   MatrixParam pt ~ a)
                => Int -> a -> Polygon pt
regularPolygon n r = Polygon $ circular $ replicate n (zeroPt .+^ (V2 0 r)) 


-- Note square and rectangle are both 'turtle drawn' and use the @iter@ 
-- functional to successively transform the current point.

-- | Create a square with bottom left corner at the origin and 
-- side-length @d@.
square :: (Num a, AffineSpace pt, ZeroPt pt, Diff pt ~ Vec2 a) 
       => a -> Polygon pt
square d = Polygon $ iter [id,f2,f3,f4] zeroPt where
  f2 = (.+^ hvec d)
  f3 = (.+^ vvec d)
  f4 = (.+^ (hvec $ negate d))
  
unitSquare :: (Num a, AffineSpace pt, ZeroPt pt, Diff pt ~ Vec2 a) 
           => Polygon pt
unitSquare = square 1 


-- | Create a rectangle with bottom-left corner at the origin and width @w@ and
-- height @h@.
rectangle :: (Num a, AffineSpace pt, ZeroPt pt, Diff pt ~ Vec2 a) 
          => a -> a -> Polygon pt
rectangle w h = Polygon $ iter [id,f2,f3,f4] zeroPt where
  f2 = (.+^ hvec w)
  f3 = (.+^ vvec h)
  f4 = (.+^ (hvec $ negate w))


-- | Create an isosceles rectangle with bottom-left corner at the 
-- origin, the base on the horizontal plane with width @bw@. The 
-- height is @h@.
isoscelesTriangle :: (Fractional a, AffineSpace pt, ZeroPt pt, Diff pt ~ Vec2 a) 
                  => a -> a -> Polygon pt
isoscelesTriangle bw h = Polygon $ sequence [id,f2,f3] zeroPt where
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
concavePolygon :: (Ord a, Num a, Real a, Floating a, 
                   AffineSpace pt, InnerSpace a, Scalar a ~ a, Diff pt ~ Vec2 a)
               => Polygon pt -> Bool
concavePolygon = any (>pi/2) . interiorAngles




--------------------------------------------------------------------------------
-- Operations


-- | Extract the interior angles of polygon.
interiorAngles :: (Num a, Real a, Floating a, 
                   AffineSpace pt, InnerSpace a, Scalar a ~ a, Diff pt ~ Vec2 a)
               => Polygon pt -> [Radian]
interiorAngles (Polygon ps) = windowedMap3c intAng ps where
  intAng a b c = interiorAngle (a .-. b) (c .-. b)




-- | Simple algorithm to deduce orientation of a polygon.
-- Will throw an error if it cannot find two successive vertices 
-- that are not collinear.
simpleOrientation :: (Collinear (pt Double), Rectangular pt) 
                  => Polygon (pt Double) -> Orientation
simpleOrientation (Polygon ps) = sign $ det $ mkMat $ noncollinear ps
  where
    noncollinear (x:y:z:xs) 
       | not (collinear x y z)  = (x,y,z)
       | otherwise              = noncollinear (y:z:xs)
    noncollinear _              = error $ "Polygon.simpleOrientation - "
                                       ++ "could not find viable vertices."

    mkMat (p1,p2,p3) = M3'3 1 x1 y1 1 x2 y2 1 x3 y3 where  
                           (P2 x1 y1::Point2 Double) = toPoint2 p1
                           P2 x2 y2 = toPoint2 p2
                           P2 x3 y3 = toPoint2 p3
                      

    sign a | a < 0     = CW
           | a > 0     = CCW
           | otherwise = error "Polygon.simpleOrientation - det=0"


-- | Reverse the orientation of the polygon (i.e. reverse 
-- the list of points that represent the polygon).
reverseOrientation :: Polygon pt -> Polygon pt
reverseOrientation = Polygon . reverse . getPolygonPoints