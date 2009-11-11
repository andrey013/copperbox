{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Curve
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Curves, circles ...
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Curve where

import Wumpus.Core
import Wumpus.Core.PictureInternal  -- TODO

import Wumpus.Geometry.Base

import Data.AffineSpace

data Curve u = Curve (Point2 u) (Point2 u) (Point2 u) (Point2 u)
  deriving (Eq,Show)

type DCurve = Curve (Point2 Double)



-- | Circle represented by center and radius
data Circle u = Circle (Point2 u) u

--------------------------------------------------------------------------------
-- Instances


instance Functor Curve where
  fmap f (Curve p0 p1 p2 p3) = 
      Curve (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)

instance Functor Circle where
  fmap f (Circle c r) = Circle (fmap f c) (f r)


instance Pointwise (Curve u) where
  type Pt (Curve u) = Point2 u
  pointwise f (Curve p0 p1 p2 p3) = Curve (f p0) (f p1) (f p2) (f p3)

instance Pointwise (Circle u) where
  type Pt (Circle u) = Point2 u
  pointwise f (Circle c r) = Circle (f c) r


instance Converse (Curve a) where
  converse (Curve p0 p1 p2 p3) = Curve p3 p2 p1 p0

--------------------------------------------------------------------------------
-- affine transformations


-- No useful scale operation on a circle - a non-uniform scale 
-- would create an ellipse.

{-

instance Scale (Circle u) where
  type ScaleUnit = u
  scale x y (Circle c r) = 
-}

--------------------------------------------------------------------------------
-- construction


-- | Create an arc - this construction is the analogue of 
-- PostScript\'s @arc@ command, but the arc is created as a 
-- Bezier curve so it should span less than 90deg.
bezierArc :: Floating u => Point2 u -> u -> Radian -> Radian -> Curve u
bezierArc pt r ang1 ang2 = Curve p0 p1 p2 p3 where
  theta = ang2 - ang1
  e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2* cos (theta/2))) 
  p0    = pt .+^ avec ang1 r
  p3    = pt .+^ avec ang2 r
  p1    = p0 .+^ avec (ang1 + pi/2) e
  p2    = p3 .+^ avec (ang2 - pi/2) e



-- | Make a circle from Bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
bezierCircle :: (Fractional u, Floating u) => Int -> Point2 u -> u -> [Curve u]
bezierCircle n pt r = para phi [] $ subdivisions (n*4) (2*pi) where
   phi a (b:_,acc) = bezierArc pt r a b : acc
   phi _ (_,acc)   = acc 

  

--------------------------------------------------------------------------------
-- operations

curveToPath1 :: Curve u -> Path u
curveToPath1 (Curve p0 p1 p2 p3) = Path p0 [PCurve p1 p2 p3]

curvesToPath :: [Curve u] -> Path u
curvesToPath []                     = error $ "curvesToPath - empty list"
curvesToPath (Curve p0 p1 p2 p3:cs) = 
   Path p0 (PCurve p1 p2 p3 : map fn cs) where 
      fn (Curve _ u v w) = PCurve u v w