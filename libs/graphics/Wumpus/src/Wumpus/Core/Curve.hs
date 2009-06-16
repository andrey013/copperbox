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


module Wumpus.Core.Curve where

import Wumpus.Core.Fun
import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.VSExtra

import Data.AffineSpace
import Data.VectorSpace

data Curve a = Curve (Point2 a) (Point2 a) (Point2 a) (Point2 a)
  deriving (Eq,Show)

type DCurve = Curve Double


instance Functor Curve where
  fmap f (Curve p0 p1 p2 p3) = 
    Curve (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)




-- de casteljau's algorithm
subdivide :: (Fractional (Scalar (Diff a)), Num (Diff a),
              VectorSpace (Diff a),  AffineSpace a)  
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

smoothw (x:xs) = smooth $ (x:xs) ++ [x]

smooth xs = intermap curver eps
  where
   pfs  = intermap3 (pipaep `ooo` proportion) xs
   eps  = combi xs pfs



combi (a:b:c:xs) (f:fs)  = fn b : combi (b:c:xs) fs 
                           where fn = f (midpoint a b) (midpoint b c)
combi _          _       = []




-- calculate Bi and return a function to be applied to midpoints
proportion :: DPoint2 -> DPoint2 -> DPoint2 -> (DPoint2 -> DPoint2)
proportion p0 p1 p2 = \p -> (p .+^ (p1 .-. p0) / (p2 .-. p1))


-- points-in-proportion-at-end-point
-- Given a function from mid-point -> Bi, generate a function that 
-- takes a end-point an returns the three control points positioned  
-- relative to the end point
pipaep :: (DPoint2 -> DPoint2) 
       -> (DPoint2 -> DPoint2 -> (DPoint2 -> (DPoint2,DPoint2,DPoint2))) 
pipaep fn  = \mp0 mp1 -> \ep -> let bi = fn mp0; v = ep .-. bi
                                in (mp0 .+^ v, ep, mp1 .+^ v)


curver :: (DPoint2,DPoint2,DPoint2) -> (DPoint2,DPoint2,DPoint2) -> DCurve
curver (_,p0,p1) (p2,p3,_) = Curve p0 p1 p2 p3

