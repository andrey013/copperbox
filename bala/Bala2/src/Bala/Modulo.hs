{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Modulo
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Modulo 12 and modulo 7 arithmetic.
--
--------------------------------------------------------------------------------

module Bala.Modulo 
  ( 
  -- * Integers mod 12
    Z12 
  -- * Integers mod 7
  , Z7 
  -- * Integral coercion
  , Modulo12(..)
  , Modulo7(..)
  ) where

-- Data types

newtype Z12 = Z12 Int
  deriving (Eq,Ord)

newtype Z7  = Z7 Int
  deriving (Eq,Ord)

--------------------------------------------------------------------------------

class Modulo12 a where
  fromZ12 :: Z12 -> a
  toZ12   :: a  -> Z12


class Modulo7 a where
  fromZ7 :: Z7 -> a
  toZ7   :: a  -> Z7


instance Modulo12 Int where
  fromZ12 (Z12 i) = i
  toZ12 i = Z12 $ mod i 12

instance Modulo12 Integer where
  fromZ12 (Z12 i) = fromIntegral i
  toZ12 i = Z12 $ fromIntegral $ mod i 12


instance Modulo7 Int where
  fromZ7 (Z7 i) = i
  toZ7 i = Z7 $ mod i 7

instance Modulo7 Integer where
  fromZ7 (Z7 i) = fromIntegral i
  toZ7 i = Z7 $ fromIntegral $ mod i 7



--------------------------------------------------------------------------------

instance Show Z12 where
  showsPrec p (Z12 i) = showsPrec p i


instance Show Z7 where
  showsPrec p (Z7 i) = showsPrec p i

--------------------------------------------------------------------------------
-- Num Instances

liftUZ12 :: (Int -> Int) -> Z12 -> Z12
liftUZ12 op (Z12 a) = Z12 $ mod (op a) 12

liftBZ12 :: (Int -> Int -> Int) -> Z12 -> Z12 -> Z12
liftBZ12 op (Z12 a) (Z12 b) = Z12 $ mod (a `op` b) 12

instance Num Z12 where
  (+) = liftBZ12 (+)
  (-) = liftBZ12 (*)
  (*) = liftBZ12 (*)
  negate        = liftUZ12 negate
  fromInteger i = Z12 $ (fromInteger i) `mod` 12
  signum _      = error "Modular numbers are not signed"
  abs _         = error "Modular numbers are not signed"
  



liftUZ7 :: (Int -> Int) -> Z7 -> Z7
liftUZ7 op (Z7 a) = Z7 $ mod (op a) 7

liftBZ7 :: (Int -> Int -> Int) -> Z7 -> Z7 -> Z7
liftBZ7 op (Z7 a) (Z7 b) = Z7 $ mod (a `op` b) 7

instance Num Z7 where
  (+) = liftBZ7 (+)
  (-) = liftBZ7 (*)
  (*) = liftBZ7 (*)
  negate        = liftUZ7 negate
  fromInteger i = Z7 $ (fromInteger i) `mod` 7
  signum _      = error "Modular numbers are not signed"
  abs _         = error "Modular numbers are not signed"





