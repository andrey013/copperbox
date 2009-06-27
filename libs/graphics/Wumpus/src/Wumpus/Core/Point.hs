{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Point
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Point type
--
--------------------------------------------------------------------------------


module Wumpus.Core.Point 
  (
  -- * Point types
    Point2(..)
  , DPoint2
  , Point3(..)
  , DPoint3

  -- * Represent a point at the origin
  , StdOrigin(..)
 
  ) where

import Wumpus.Core.Pointwise

--------------------------------------------------------------------------------
-- Point types and standard instances

data Point2 a = P2 !a !a
  deriving (Eq,Show)

type DPoint2 = Point2 Double


data Point3 a = P3 !a !a !a
  deriving (Eq,Show)

type DPoint3 = Point3 Double



instance Functor Point2 where
  fmap f (P2 a b) = P2 (f a) (f b)

instance Functor Point3 where
  fmap f (P3 a b c) = P3 (f a) (f b) (f c)


instance Pointwise (Point2 a) where
  type Pt (Point2 a) = Point2 a
  pointwise f pt = f pt


instance Pointwise (Point3 a) where
  type Pt (Point3 a) = Point3 a
  pointwise f pt = f pt


--------------------------------------------------------------------------------
-- Represent a point at the origin

-- | Construct a point at the origin. @zeroPt@ should generally be considered
-- a synonym for the origin (but it can be overriden).
class StdOrigin pt where
  stdOrigin :: pt
  zeroPt    :: pt
  zeroPt    = stdOrigin

instance Num a => StdOrigin (Point2 a) where
  stdOrigin = P2 0 0
  zeroPt    = P2 0 0 


instance Num a => StdOrigin (Point3 a) where
  stdOrigin = P3 0 0 0
  zeroPt    = P3 0 0 0


