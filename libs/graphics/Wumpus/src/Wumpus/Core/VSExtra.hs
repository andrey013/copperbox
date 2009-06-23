{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.VSExtra
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extra functions built on the Vector Space lib.
--
--------------------------------------------------------------------------------


module Wumpus.Core.VSExtra where

import Data.AffineSpace
import Data.VectorSpace


midpoint :: (Fractional (Scalar (Diff p)), AffineSpace p, VectorSpace (Diff p))
         => p -> p -> p
midpoint p0 p1 = p0 .+^ v1 ^/ 2 where v1 = p1 .-. p0


adjustvk :: (Fractional (Scalar (Diff p)), AffineSpace p, VectorSpace (Diff p))
         => p -> p -> p -> Scalar (Diff p) -> (p,p)
adjustvk p0 p1 p2 k = (p1 .+^ vl,p1 .+^ vr) where
  vl = (p0 .-. p1) ^* k
  vr = (p2 .-. p1) ^* k


isZeroV :: (AdditiveGroup v,Eq v) => v -> Bool
isZeroV = (==) zeroV