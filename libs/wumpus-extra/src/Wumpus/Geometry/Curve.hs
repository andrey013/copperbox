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
-- Cubic Bezier curves ...
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Curve 
  (
  -- * Data types
    CubicBezier(..)
  , DCubicBezier

  -- * Operations
  , bezierArc
  , bezierCircle

  , curveToPath
  , curvesToPath

  ) where

import Wumpus.Core

import Wumpus.Geometry.Base
import Wumpus.Geometry.CoreAdditions    -- TEMPORARY

import Data.AffineSpace

data CubicBezier u = CubicBezier (Point2 u) (Point2 u) (Point2 u) (Point2 u)
  deriving (Eq,Show)

type DCubicBezier = CubicBezier (Point2 Double)



--------------------------------------------------------------------------------
-- Instances


instance Functor CubicBezier where
  fmap f (CubicBezier p0 p1 p2 p3) = 
      CubicBezier (fmap f p0) (fmap f p1) (fmap f p2) (fmap f p3)



instance Pointwise (CubicBezier u) where
  type Pt (CubicBezier u) = Point2 u
  pointwise f (CubicBezier p0 p1 p2 p3) = CubicBezier (f p0) (f p1) (f p2) (f p3)



instance Converse (CubicBezier u) where
  converse (CubicBezier p0 p1 p2 p3) = CubicBezier p3 p2 p1 p0

--------------------------------------------------------------------------------
-- affine transformations


--------------------------------------------------------------------------------
-- construction


-- | Create an arc - this construction is the analogue of 
-- PostScript\'s @arc@ command, but the arc is created as a 
-- Bezier curve so it should span less than 90deg.
bezierArc :: Floating u => Point2 u -> u -> Radian -> Radian -> CubicBezier u
bezierArc pt r ang1 ang2 = CubicBezier p0 p1 p2 p3 where
  theta = ang2 - ang1
  e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2* cos (theta/2))) 
  p0    = pt .+^ avec ang1 r
  p3    = pt .+^ avec ang2 r
  p1    = p0 .+^ avec (ang1 + pi/2) e
  p2    = p3 .+^ avec (ang2 - pi/2) e



-- | Make a circle from Bezier curves - @n@ is the number of 
-- subdivsions per quadrant.
bezierCircle :: (Fractional u, Floating u) 
             => Int -> Point2 u -> u -> [CubicBezier u]
bezierCircle n pt r = para phi [] $ subdivisions (n*4) (2*pi) where
   phi a (b:_,acc) = bezierArc pt r a b : acc
   phi _ (_,acc)   = acc 

  

--------------------------------------------------------------------------------
-- operations

curveToPath :: CubicBezier u -> Path u
curveToPath (CubicBezier p0 p1 p2 p3) = path p0 [curveTo p1 p2 p3]



curvesToPath :: [CubicBezier u] -> Path u
curvesToPath []                     = error $ "curvesToPath - empty list"
curvesToPath (CubicBezier p0 p1 p2 p3:cs) = 
   path p0 (curveTo p1 p2 p3 : map fn cs) where 
      fn (CubicBezier _ u v w) = curveTo u v w