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


module Wumpus.Core.Point where

-- import Data.AffineSpace
-- import Data.VectorSpace

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


