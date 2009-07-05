{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
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
-- Extra functions built on the Vector Space lib with no dependencies on 
-- the Wumpus data types and typeclasses with no Wumpus dependencies.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Geometric
  (
  -- * Type classes
    Congruent(..)
  , Converse(..)

  , ExtractPoints(..)
  
  -- * Functions
  , midpoint
  , adjustvk
  , isZeroV
  ) where

import Data.AffineSpace
import Data.VectorSpace

--------------------------------------------------------------------------------
-- Type classes

class Congruent a where
  congruent :: a -> a -> Bool

-- | Reverse the direction of some ordered object (line, polyline etc).
class Converse a where 
  converse :: a -> a


-- | Extract points from a polygon, polyline etc..
class ExtractPoints t where
  type Pnt t :: *
  extractPoints :: t -> [Pnt t]
  endPoint      :: t -> Pnt t



--------------------------------------------------------------------------------
-- Functions


-- | midpoint between two points
midpoint :: (Fractional (Scalar (Diff p)), AffineSpace p, VectorSpace (Diff p))
         => p -> p -> p
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


-- | @p0@, @p1@ and @p2@ represent 3 points on a line, @adjustvk@ redraws 
-- @p0@ and @p2@ to be /nearer/ @p1@ by the scaling factor @k@.
adjustvk :: (Fractional (Scalar (Diff p)), AffineSpace p, VectorSpace (Diff p))
         => p -> p -> p -> Scalar (Diff p) -> (p,p)
adjustvk p0 p1 p2 k = (p1 .+^ vl,p1 .+^ vr) where
  vl = (p0 .-. p1) ^* k
  vr = (p2 .-. p1) ^* k

-- | Predicate to test if the vector is zero.
isZeroV :: (AdditiveGroup v,Eq v) => v -> Bool
isZeroV = (==) zeroV