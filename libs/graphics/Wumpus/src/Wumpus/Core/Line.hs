{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Line
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Line segment
--
--------------------------------------------------------------------------------


module Wumpus.Core.Line where

import Wumpus.Core.Instances
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Data.AffineSpace

-- To determine...
-- (pt x vec) or (pt x pt)?


data LineSegment a = LS (Point2 a) (Vec2 a)
  deriving (Eq,Show)

type DLineSegment = LineSegment Double


instance Functor LineSegment where
  fmap f (LS pt vec) = LS (fmap f pt) (fmap f vec)

lineTo :: (Num a, AffineSpace a, Diff a ~ a) 
       => Point2 a -> Point2 a -> LineSegment a
lineTo p1 p2 = LS p1 v where v = p2 .-. p1

