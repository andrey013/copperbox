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



instance Functor Point2 where
  fmap f (P2 a b) = P2 (f a) (f b)



origin :: Num a => Point2 a
origin = P2 0 0

zeroPt :: Num a => Point2 a
zeroPt = P2 0 0 



