{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Radian
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Radian
--
--------------------------------------------------------------------------------


module Wumpus.Core.Radian
  (
  -- * Radian type
    Radian(..)
  , DRadian

  -- * Operations 
  -- ** Conversion
  , d2r
  , r2d

  ) where

import Control.Applicative

--------------------------------------------------------------------------------
-- Radian as a distinct type, parametric on numberic type

newtype Radian a = Radian { fromRadian :: a }
  deriving (Eq,Ord,Show,Num,Real,Fractional,Floating,RealFrac,RealFloat)


instance Functor Radian where
  fmap f (Radian a) = Radian (f a)


instance Applicative Radian where
  pure = Radian
  (<*>) (Radian f) (Radian a) = Radian (f a)



type DRadian = Radian Double


--------------------------------------------------------------------------------


-- degrees / radians

d2r :: Floating a => a -> Radian a 
d2r = Radian . (*) (pi/180)

r2d :: Floating a => Radian a -> a
r2d = (*) (180/pi) . fromRadian
