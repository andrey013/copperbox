{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Geometric
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extra datatypes, functions and classes (some built on the 
-- VectorSpace lib), no dependencies other Wumpus modules.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Geometric
  (
  -- * Types
    Orientation(..)


  -- * Type classes
  , Congruent(..)
  , Converse(..)

  , HasPoints(..)
  
  -- * Functions
  , midpoint
  , adjustvk
  , isZeroV
  ) where

import Data.AffineSpace
import Data.VectorSpace

--------------------------------------------------------------------------------

data Orientation = CW | CCW
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Type classes


-- | Have two objects the same size and shape?
class Congruent a where
  congruent :: a -> a -> Bool

-- | Reverse the direction of some ordered object (line, polyline etc).
class Converse a where 
  converse :: a -> a


-- | Extract points from a polygon, polyline etc..
class HasPoints t where
  type Pnt t :: *
  extractPoints :: t -> [Pnt t]
  startPoint    :: t -> Pnt t
  endPoint      :: t -> Pnt t



instance HasPoints (t a) => HasPoints [t a] where
  type Pnt [t a] = Pnt (t a)
  extractPoints = concatMap extractPoints 
  startPoint = head . extractPoints
  endPoint = last . extractPoints


--------------------------------------------------------------------------------
-- Functions


-- | midpoint between two points
midpoint :: (Fractional a, AffineSpace p, VectorSpace v, Diff p ~ v, Scalar v ~ a)
         => p -> p -> p
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


-- | @p0@, @p1@ and @p2@ represent 3 points on a line, @adjustvk@ redraws 
-- @p0@ and @p2@ to be /nearer/ @p1@ by the scaling factor @k@.
adjustvk :: (Fractional a, AffineSpace p, VectorSpace v, Diff p ~ v, Scalar v ~ a)
         => p -> p -> p -> a -> (p,p)
adjustvk p0 p1 p2 k = (p1 .+^ vl,p1 .+^ vr) where
  vl = (p0 .-. p1) ^* k
  vr = (p2 .-. p1) ^* k

-- | Predicate to test if the vector is zero.
isZeroV :: (AdditiveGroup v,Eq v) => v -> Bool
isZeroV = (==) zeroV


