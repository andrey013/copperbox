{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
  
  -- * Construction
  , toRadian

  -- * Predicates
  , complementary

  -- * Operations 
  , Direction2(..)

  , interior

  -- ** Conversion
  , d2r
  , r2d

  ) where


import Control.Applicative

--------------------------------------------------------------------------------
-- Radian as a distinct type, parametric on numberic type

newtype Radian a = Radian { fromRadian :: a }
  deriving (Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat)


instance Show a => Show (Radian a) where
 showsPrec i (Radian a) = showsPrec i a


instance Functor Radian where
  fmap f (Radian a) = Radian (f a)


instance Applicative Radian where
  pure = Radian
  (<*>) (Radian f) (Radian a) = Radian (f a)



type DRadian = Radian Double

--------------------------------------------------------------------------------
-- Construction

toRadian :: a -> Radian a
toRadian = Radian



--------------------------------------------------------------------------------
-- NOTE - The numeric types and their conversions need working out.
-- Are /toRational/ hacks like the one in complementary to get (==) valid?
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Predicates


complementary :: (Real a,Floating a) => Radian a -> Radian a -> Bool
complementary (Radian a) (Radian b) = toRational (a+b) == toRational (pi/2::Double)



--------------------------------------------------------------------------------
-- Operations


-- direction in 2D space

class Direction2 t where   
  direction2 :: Floating a => t a -> Radian a

-- | The interior between two angles - shortest distance (negative cw) or 
-- (positive ccw).
interior :: (Real a, Fractional a, Ord a) => Radian a -> Radian a -> Radian a
interior a b = fn (b-a)
  where 
    fn d = if (realToFrac $ d) > pi then ((fromRational $ 2* toRational pi) - d) else d


-- degrees / radians

d2r :: Floating a => a -> Radian a 
d2r = Radian . (*) (pi/180)

r2d :: Floating a => Radian a -> a
r2d = (*) (180/pi) . fromRadian
