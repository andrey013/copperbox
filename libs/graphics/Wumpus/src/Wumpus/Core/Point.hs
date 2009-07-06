{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  , WtPoint(..)

  -- * Construction
  , ZeroPt(..)
  
  -- * Predicates 
  , collinear

  -- * Affine combination
  , (|+|)  
  , affineSum

  ) where

import Wumpus.Core.Pointwise

import Data.AffineSpace
import Data.VectorSpace


--------------------------------------------------------------------------------
-- Point types and standard instances

data Point2 a = P2 !a !a
  deriving (Eq,Show)

type DPoint2 = Point2 Double


data Point3 a = P3 !a !a !a
  deriving (Eq,Show)

type DPoint3 = Point3 Double


-- Weighted points
data WtPoint (pt :: * -> *) a = WP a (pt a)
  deriving (Eq,Show)



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
-- Construction

-- Represent a point at the origin

-- | Construct a point at the origin. 
class ZeroPt pt where
  zeroPt    :: pt


instance Num a => ZeroPt (Point2 a) where
  zeroPt    = P2 0 0 


instance Num a => ZeroPt (Point3 a) where
  zeroPt    = P3 0 0 0


--------------------------------------------------------------------------------
-- Predicates

-- | Are the three points in the same line?
collinear :: Real a => Point2 a -> Point2 a -> Point2 a -> Bool
collinear (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) = rat1 == rat2
  where 
    x1' = toRational x1
    y1' = toRational y1
    
    rat1 = ((toRational y2)-y1') / ((toRational x2)-x1')
    rat2 = ((toRational y3)-y1') / ((toRational x3)-x1')



--------------------------------------------------------------------------------
-- Affine combination

infixl 6 |+|


-- | Affine combination of two weighted points.
-- Note the sum of the weights a1 and a2 must satisfy: @a1 + a2 = 1@


(|+|) :: (a ~ Diff (pt (Scalar a)),
          Real (Scalar a), 
          AffineSpace (pt (Scalar a)), 
          VectorSpace a) 
      => WtPoint pt (Scalar a) -> WtPoint pt (Scalar a) -> pt (Scalar a)

(WP a1 p1) |+| (WP a2 p2) 
    | toRational (a1+a2) == 1 = p1 .+^ (a2 *^ (p2 .-. p1))
    | otherwise               = error "affine combination: weights do not sum to 1" 



-- | Affine combination summing a list of weighted points.
-- Note the weights must sum to 1.

affineSum :: (a ~ Diff (pt (Scalar a)),
              Real (Scalar a), 
              AffineSpace (pt (Scalar a)), 
              VectorSpace a)  
          => [WtPoint pt (Scalar a)] -> pt (Scalar a)
affineSum []              = error "affineSum: empty"
affineSum (WP a1 p1 : xs) = post $ foldr fn (a1, p1) xs 
  where 
    fn (WP an pn) (tot,pt) = (tot+an, pt .+^ (an *^ (pn .-. p1)))
    post (a,p) | toRational a == 1  = p
               | otherwise          = error "affineSum: weights do not sum to 1"


