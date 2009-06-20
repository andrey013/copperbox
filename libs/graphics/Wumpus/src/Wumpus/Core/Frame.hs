{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Frame
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Frames
--
--------------------------------------------------------------------------------


module Wumpus.Core.Frame where

import Wumpus.Core.Instances
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

data Frame2 a = Frame2 (Point2 a) (Vec2 a) (Vec2 a) 
  deriving (Eq,Show)


class Ortho fr where
  type Point fr :: *
  ortho :: Point fr -> fr


instance Num a => Ortho (Frame2 a) where
  type Point (Frame2 a) = Point2 a
  ortho ogin = Frame2 ogin (V2 1 0) (V2 0 1)


piw :: DPoint2 -> Frame2 Double -> DPoint2
piw = pointInWorld


-- Given a point and a frame, return the point in world coordinates
pointInWorld :: (Num a, AffineSpace a, VectorSpace a, 
                 Scalar a ~ a, a ~ Diff a)
             => Point2 a -> Frame2 a -> Point2 a
pointInWorld (P2 x y) (Frame2 o xv yv) = (o .+^ xv') .+^ yv'
  where 
    xv' = x *^ xv
    yv' = y *^ yv

