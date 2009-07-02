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
  
  -- * de Casteljau\'s algorithm
  , subdivide

  -- * Shemanarev\'s smoothing algorithm
  , smoothw

  -- * Bezier curve of circle segment
  , circleSegment

  , bezierArc

  ) where

import Wumpus.Core.Fun
import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.Pointwise
import Wumpus.Core.Radian
import Wumpus.Core.Vector
import Wumpus.Core.VSExtra

import Data.AffineSpace
import Data.VectorSpace

--------------------------------------------------------------------------------
-- Curve types and standard instances

data Curve a = Curve (Point2 a) (Point2 a) (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DCurve = Curve Double


instance Functor Curve where
  fmap f (Curve p0 p1 p2 p3) = 
    Curve (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)


instance Pointwise (Curve a) where
  type Pt (Curve a) = Point2 a
  pointwise f (Curve p0 p1 p2 p3) = Curve (f p0) (f p1) (f p2) (f p3)


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
circleSegment :: (Floating a, AffineSpace a) => a -> Curve a
circleSegment ang = Curve p0 p1 p2 p3 where
  k  = (4/3) * tan (ang / 4)
  p0 = P2 1 0
  p3 = P2 (cos ang) (sin ang)
  p1 = P2 1 k
  p2 = p3 .+^ (V2 (k * sin ang) (-k * cos ang)) 


--
bezierArc :: (Floating a, AffineSpace a) 
          => a -> Radian a -> Radian a -> Curve a
bezierArc r ang1 ang2 = Curve p0 p1 p2 p3 where
  theta = ang2 - ang1
  e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2* cos (theta/2))) 
  p0    = zeroPt .+^ vec2 ang1 r
  p3    = zeroPt .+^ vec2 ang2 r
  p1    = p0 .+^ vec2 (ang1 + pi/2) e
  p2    = p3 .+^ vec2 (ang2 - pi/2) e
