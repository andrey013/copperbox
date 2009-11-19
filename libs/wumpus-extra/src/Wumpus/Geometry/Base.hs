{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Base
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Type classes and common things.
--
--------------------------------------------------------------------------------


module Wumpus.Geometry.Base 
  (
  -- * Type classes
    Converse(..)
  , CCWAngle(..)
  , ExtractPath(..)
  , ObjectLength(..)
  , Midpoint(..)

  -- * Operations
  , subdivisions
  , midpointBetween
  , para
  , circular
  , circularAbout

  ) where

import Wumpus.Core

import Data.AffineSpace
import Data.VectorSpace



--------------------------------------------------------------------------------
-- Type classes

-- | Reverse the direction of some ordered object (line segment, 
-- bezier curve, ...).
--
class Converse a where 
  converse :: a -> a

-- | Counter-clockwise angle formed with the horizontal x-axis.
--
class CCWAngle a where ccwAngle :: a -> Radian


-- | Extract the path from some object that can be /traced/.
class ExtractPath a where 
  extractPath :: a -> Path (DUnit a)

-- | Length of object (e.g. LineSegment).
class ObjectLength a where 
  objectLength :: a -> (DUnit a)

class Midpoint a where
  midpoint :: a -> Point2 (DUnit a)



--------------------------------------------------------------------------------
-- Common ...


-- Subdivide a positive non-zero fractional number n times 
-- returning the list @[0, a/n, 2*a/n, ..., a]@  
subdivisions :: Fractional a => Int -> a -> [a]
subdivisions i a = take (i+1) $ iterate (+n) 0 where 
   n = a / fromIntegral i


-- | Midpoint between two points.
midpointBetween :: Fractional u => Point2 u -> Point2 u -> Point2 u
midpointBetween p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


--------------------------------------------------------------------------------


-- | paramorphism (generalizes cata (foldr), folds right...)
para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para phi b = step
  where step []     = b
        step (x:xs) = phi x (xs, step xs)


-- | @ circle n r @ 
-- Trace @n@ equally spaced points around a circle of radius @r@
-- centered at the origin. The points proceed counter-clockwise 
-- from the the initial point on the x-axis.
circular :: (Floating u , Real u) => Int -> u -> [Point2 u]
circular = circularAbout zeroPt


-- | @ circularAbout pt n r ...@
-- Trace @n@ equally spaced points around a circle of radius @r@
-- centered at @pt@. The points proceed counter-clockwise 
-- from the the initial point on the x-axis.
--
circularAbout :: (Floating u , Real u) 
              => Point2 u -> Int -> u -> [Point2 u]
circularAbout pt n r = take n $ iterate (rotateAbout ang pt) px
  where
    ang  = let n'::Double = fromIntegral n in toRadian $  2*pi/ n'
    px   = pt .+^ V2 0 r

