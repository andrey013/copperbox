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
    Radian
  
  -- * Construction
  , toRadian

  -- * Extraction
  , fromRadian

  -- * Predicates
  , complementary
  , obtuse
  , rightAngle

  -- * Operations 
  , Direction2(..)

  , interior

  -- ** Conversion
  , d2r
  , r2d

  ) where


radian_epsilon :: Double
radian_epsilon = 0.0001

--------------------------------------------------------------------------------

-- | Radian is represented with a distinct type. 
-- E quality and ordering are approximate where the epsilon is 0.0001.
newtype Radian = Radian { getRadian :: Double }
  deriving (Num,Real,Fractional,Floating,RealFrac,RealFloat)


instance Show Radian where
 showsPrec i (Radian a) = showsPrec i a

instance Eq Radian where (==) = req

instance Ord Radian where
  compare a b | a `req` b = EQ
              | otherwise = getRadian a `compare` getRadian b


--------------------------------------------------------------------------------
-- Construction

toRadian :: Real a => a -> Radian 
toRadian = Radian . realToFrac


-- Extraction 

fromRadian :: Fractional a => Radian -> a
fromRadian = realToFrac . getRadian


--------------------------------------------------------------------------------
-- Predicates

req :: Radian -> Radian -> Bool
req a b = (fromRadian $ abs (a-b)) < radian_epsilon

rlt :: Radian -> Radian -> Bool
rlt a b = (a<b) && not (a `req` b)

rgt :: Radian -> Radian -> Bool
rgt a b = (a>b) && not (a `req` b)

complementary :: Radian -> Radian -> Bool
complementary a b = (a+b)`req` (pi/2)

obtuse :: Radian -> Bool
obtuse a = (pi/2) `rlt` a && a `rlt` pi

rightAngle :: Radian -> Bool
rightAngle a = a `req` (pi/2)

--------------------------------------------------------------------------------
-- Operations


-- direction in 2D space

class Direction2 t where   
  direction2 :: (Real a, Floating a) => t a -> Radian

-- | The interior between two angles - shortest distance (negative cw) or 
-- (positive ccw).
interior :: Radian -> Radian -> Radian
interior a b = fn (b-a)
  where 
    fn d = if d `rgt` pi then (2*pi)-d else d


-- degrees / radians

d2r :: (Floating a, Real a) => a -> Radian
d2r = Radian . realToFrac . (*) (pi/180)

r2d :: (Floating a, Real a) => Radian -> a
r2d = (*) (180/pi) . fromRadian


