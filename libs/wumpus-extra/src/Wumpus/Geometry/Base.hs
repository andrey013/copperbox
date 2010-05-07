{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Geometry.Base
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
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
-- Instances for Wumpus core

instance Converse Radian where
  converse = norm . (+pi)
    where norm = id -- to do
         



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

