{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Symbolic.Basis.Z7
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Z7 modular integer type.
--
--------------------------------------------------------------------------------

module Majalan.Symbolic.Basis.Z7
  ( 
  -- * Datatype
    Z7

  -- * Conversion type class
  , Modulo7(..)

  ) where


import Data.AdditiveGroup               -- package: vector-space

newtype Z7 = Z7 Int
  deriving (Enum,Eq,Integral,Ord,Real)


instance Show Z7 where
  showsPrec p (Z7 i) = showsPrec p i


liftUZ7 :: (Int -> Int) -> Z7 -> Z7
liftUZ7 op (Z7 a) = Z7 $ mod (op a) 7

liftBZ7 :: (Int -> Int -> Int) -> Z7 -> Z7 -> Z7
liftBZ7 op (Z7 a) (Z7 b) = Z7 $ mod (a `op` b) 7

instance Num Z7 where
  (+) = liftBZ7 (+)
  (-) = liftBZ7 (-)
  (*) = liftBZ7 (*)
  negate        = liftUZ7 negate
  fromInteger i = Z7 $ (fromInteger i) `mod` 7
  signum _      = error "Modular numbers are not signed"
  abs _         = error "Modular numbers are not signed"




--------------------------------------------------------------------------------

class Modulo7 a where
  fromZ7 :: Z7 -> a
  toZ7   :: a  -> Z7


instance Modulo7 Int where
  fromZ7 (Z7 i) = i
  toZ7 i = Z7 $ mod i 7

instance Modulo7 Integer where
  fromZ7 (Z7 i) = fromIntegral i
  toZ7 i = Z7 $ fromIntegral $ mod i 7

--------------------------------------------------------------------------------


instance AdditiveGroup Z7 where
  zeroV = 0
  (^+^) = (+)
  negateV v = 0 - v