{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Point
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


module Graphics.Wumpus.Point where



-- import Data.AffineSpace
-- import Data.VectorSpace


data Point2 a = P2 !a !a
  deriving (Eq,Show)

type DPoint2 = Point2 Double


-- Hmm, do Points have a genuine (+) operation?

instance Num a => Num (Point2 a) where
  (+) (P2 a b) (P2 x y) = P2 (a+x) (b+y)
  (-) (P2 a b) (P2 x y) = P2 (a-x) (b-y)
  (*) (P2 a b) (P2 x y) = P2 (a*x) (b*y)
  abs (P2 a b)          = P2 (abs a) (abs b)
  negate (P2 a b)       = P2 (negate a) (negate b)
  signum (P2 a b)       = P2 (signum a) (signum b)
  fromInteger i         = P2 (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (Point2 a) where
  (/) (P2 a b) (P2 x y) = P2 (a/x) (b/y)
  recip (P2 a b)        = P2 (recip a) (recip b)
  fromRational a        = P2 (fromRational a) (fromRational a)







