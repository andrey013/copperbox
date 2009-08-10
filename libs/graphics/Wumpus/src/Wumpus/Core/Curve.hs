{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Curve
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Cubic bezier curves
--
--------------------------------------------------------------------------------


module Wumpus.Core.Curve 
  ( 
  -- * Curve types  
    Curve(..)
  , DCurve

  -- * Construction
  , tildeCurve
  
  -- * de Casteljau\'s algorithm
  , subdivide
  , subdividet

  -- * Shemanarev\'s smoothing algorithm
  , smoothw

  -- * Bezier curve of circle segment
  , circleSegment

  , bezierArc

  , bezierCircle


  -- * tangents
  , startTangent
  , endTangent
  , startTangentVector
  , endTangentVector

  , cubic
  , gravesenLength


  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Geometric
import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace


default (Integer,Rational,Double)

--------------------------------------------------------------------------------
-- Curve types and standard instances

data Curve a = Curve (Point2 a) (Point2 a) (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DCurve = Curve Double

-- type CoCurve a = Point2 a -> Curve a

-- type DCoCurve = CoCurve Double


instance Functor Curve where
  fmap f (Curve p0 p1 p2 p3) = 
    Curve (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)



-- Geometrical instances

instance Pointwise (Curve a) where
  type Pt (Curve a) = Point2 a
  pointwise f (Curve p0 p1 p2 p3) = Curve (f p0) (f p1) (f p2) (f p3)


instance HasPoints (Curve a) where
  type Pnt (Curve a) = Point2 a
  extractPoints (Curve p0 p1 p2 p3) = [p0,p1,p2,p3]
  endPoint (Curve _ _ _ p3)         = p3
  startPoint (Curve p0 _ _ _)       = p0


instance Converse (Curve a) where
  converse (Curve p0 p1 p2 p3) = Curve p3 p2 p1 p0


--------------------------------------------------------------------------------
-- Construction

-- | Create a tilde (sinusodial) curve about the horizontal plane.

tildeCurve :: (Floating a, AffineSpace a, Converse (Vec2 a)) 
           => a -> (Point2 a -> Curve a)
tildeCurve w = \pt -> let endpt = pt .+^ hvec w
                      in Curve pt (pt .+^ v) (endpt .+^ converse v) endpt
  where 
    v = avec2 (pi/4) (w/2)


--------------------------------------------------------------------------------
-- operations

-- de Casteljau's algorithm
subdivide :: (Fractional (Scalar a), Num a, VectorSpace a,  AffineSpace a)  
          => Curve a -> (Curve a, Curve a)
subdivide (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = midpoint p0    p1
    p12   = midpoint p1    p2
    p23   = midpoint p2    p3
    p012  = midpoint p01   p12
    p123  = midpoint p12   p23
    p0123 = midpoint p012  p123



subdividet :: (a ~ Scalar a, Fractional a, Real i,
               VectorSpace a,  AffineSpace a)  
           => i -- Scalar (Vec2 a) 
           -> Curve (Scalar (Vec2 a)) 
           -> (Curve (Scalar (Vec2 a)), Curve (Scalar (Vec2 a)))
subdividet t (Curve p0 p1 p2 p3) = 
    (Curve p0 p01 p012 p0123, Curve p0123 p123 p23 p3)
  where
    p01   = affc p0    p1
    p12   = affc p1    p2
    p23   = affc p2    p3
    p012  = affc p01   p12
    p123  = affc p12   p23
    p0123 = affc p012  p123
    
    affc pa pb = (WP (1-t) pa) |+| (WP t pb)

----

-- Shemanarev's algorithm

smoothw :: Double -> [DPoint2] -> [DCurve]
smoothw k xs = take (length xs) $ smoothBase k (cycle xs)

smoothBase :: Double -> [DPoint2] -> [DCurve]
smoothBase k xs = intermap curver eps
  where
   pfs  = intermap3 (pipaep k `ooo` proportion) xs
   eps  = combi xs pfs


combi :: (Fractional (Scalar (Diff t)), AffineSpace t, VectorSpace (Diff t))
      => [t] -> [t -> t -> t -> a] -> [a]
combi (a:b:c:xs) (f:fs)  = fn b : combi (b:c:xs) fs 
                           where fn = f (midpoint a b) (midpoint b c)
combi _          _       = []



-- calculate Bi and return a function to be applied to midpoints
proportion :: DPoint2 -> DPoint2 -> DPoint2 -> (DPoint2 -> DPoint2 -> DPoint2)
proportion p0 p1 p2 = \mp0 mp1 -> let l1  = p1 .-. p0
                                      l2  = p2 .-. p1
                                      r   = magnitude l1 / magnitude l2  
                                      l01 = mp1 .-. mp0                                      
                                  in mp0 .+^ (l01 ^/ (2*r))


-- points-in-proportion-at-end-point
-- Given a function from mid-point -> Bi, generate a function that 
-- takes a end-point an returns the three control points positioned  
-- relative to the end point
pipaep :: Double -> (DPoint2 -> DPoint2 -> DPoint2) 
       -> (DPoint2 -> DPoint2 -> (DPoint2 -> (DPoint2,DPoint2,DPoint2))) 
pipaep k fn  = \mp0 mp1 -> \ep -> let bi  = fn mp0 mp1 
                                      v   = ep  .-. bi
                                      p00 = mp0 .+^ v
                                      p01 = mp1 .+^ v
                                      (p00',p01')= adjustvk p00 ep p01 k
                                  in (p00',ep,p01')



curver :: (DPoint2,DPoint2,DPoint2) -> (DPoint2,DPoint2,DPoint2) -> DCurve
curver (_,p0,p1) (p2,p3,_) = Curve p0 p1 p2 p3


--------------------------------------------------------------------------------


-- Acknowledgment - this appears due to Gernot Hoffmann
-- ang should be less then 90o (pi/2)
circleSegment :: (Floating a, AffineSpace a) => Radian -> Curve a
circleSegment ang = Curve p0 p1 p2 p3 where
  k  = (4/3) * tan (ang / 4)
  p0 = P2 1 0
  p3 = P2 (fromRadian $ cos ang) (fromRadian $ sin ang)
  p1 = P2 1 (fromRadian k)
  p2 = p3 .+^ (V2 (fromRadian $ k * sin ang) (fromRadian $ -k * cos ang)) 


--

bezierArc :: Floating a => a -> Radian -> Radian -> Curve a
bezierArc r ang1 ang2 = Curve p0 p1 p2 p3 where
  theta = ang2 - ang1
  e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2* cos (theta/2))) 
  p0    = zeroPt .+^ avec2 ang1 r
  p3    = zeroPt .+^ avec2 ang2 r
  p1    = p0 .+^ avec2 (ang1 + pi/2) e
  p2    = p3 .+^ avec2 (ang2 - pi/2) e



-- | Make a circle from bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
bezierCircle :: Floating a => Int -> a -> [Curve a]
bezierCircle n r = map (\(a,b) -> bezierArc r a b) quads
  where
    quads :: [(Radian,Radian)]
    quads   = subs (n*4) (2 * pi)   -- 4 times n for all 4 quadrants




subs :: Fractional a => Int -> a -> [(a,a)]
subs n a = zip xs ys
  where
    a' = a / fromIntegral n
    ds = replicate (n-1) a'
    xs = scanl (+) 0  ds
    ys = scanl (+) a' ds


--------------------------------------------------------------------------------
-- Tangents

startTangent :: (Ord a, Floating a, Real a, 
                 AffineSpace a, InnerSpace a, a ~ Scalar a) 
             => Curve a -> Radian
startTangent = vangle . startTangentVector


endTangent :: (Ord a, Floating a, Real a, 
               AffineSpace a, InnerSpace a, a ~ Scalar a) 
           => Curve a -> Radian
endTangent = vangle . endTangentVector



startTangentVector :: (Ord a, Floating a, 
                       AffineSpace a, InnerSpace a, a ~ Scalar a) 
                   => Curve a -> Vec2 a
startTangentVector (Curve p0 p1 _ _) = freeVector p0 p1


endTangentVector :: (Ord a, Floating a, 
                     AffineSpace a, InnerSpace a, a ~ Scalar a) 
                 => Curve a -> Vec2 a
endTangentVector (Curve _ _ p2 p3) = freeVector p3 p2


--------------------------------------------------------------------------------









-- | Weighted point on a bezier curve - via the famous cubic bezier formula.

cubic :: (a ~ Scalar a, Fractional a, Real a, AffineSpace a, VectorSpace a) 
      =>  Curve (Scalar (Vec2 a)) -> Scalar (Vec2 a) -> Point2 (Scalar (Vec2 a))

cubic (Curve p0 p1 p2 p3) t = affineSum [WP w0 p0, WP w1 p1, WP w2 p2, WP w3 p3]
  where
    w0 = (1-t)^(3::Integer)
    w1 = 3*t*(1-t)^(2::Integer)
    w2 = 3 * (t^(2::Integer)) * (1-t)
    w3 = t^(3::Integer)


controlPolygonLength :: (Floating a,  AffineSpace a, 
                         InnerSpace a, a ~ Scalar a) 
                     => Curve a -> a
controlPolygonLength (Curve p0 p1 p2 p3) = 
  distance p0 p1 + distance p1 p2 + distance p2 p3

cordLength :: (Floating a,  AffineSpace a, InnerSpace a, a ~ Scalar a) 
           => Curve a -> a
cordLength (Curve p0 _ _ p3) = distance p0 p3

-- | Gravesen\'s bezier arc-length approximation. 
-- Note this implementation is parametrized on error tolerance.
gravesenLength :: (Floating a, Ord a, AffineSpace a, 
                   InnerSpace a, a ~ Scalar a) 
               => a -> Curve a -> a
gravesenLength err_tol crv = step crv where
  step c = let l1 = controlPolygonLength c 
               l0 = cordLength c
           in if   l1-l0 > err_tol
              then let (a,b) = subdivide c in step a + step b
              else 0.5*l0 + 0.5*l1




