{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tactus.Fraction
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Fraction datatype - alternative to Data.Ratio that is not
-- eager to reduce to proper fractions.
--
--------------------------------------------------------------------------------


module Tactus.Fraction
  (
    Fraction(..)

  ) where

import Data.Ratio

infixl 7 :%:

data Fraction = Integer :%: Integer
  deriving (Eq,Show)

toR :: Fraction -> Rational
toR (n :%: d) = n % d

fromR :: Rational -> Fraction
fromR r = (n :%: d) where (n,d) = (numerator r, denominator r)

instance Ord Fraction where
  compare a b = (toR a) `compare` (toR b)

liftFrac2 :: (Integer -> Integer -> Integer, Rational -> Rational -> Rational) 
          -> Fraction -> Fraction -> Fraction
liftFrac2 (f,_) (n :%: d) (n' :%: d') | d == d'   = (n `f` n') :%: d
liftFrac2 (_,g) a          b                      = fromR (toR a `g` toR b)


instance Num Fraction where
  (+) = liftFrac2 ((+),(+))
  (-) = liftFrac2 ((-),(-))
  (*) = liftFrac2 ((*),(*))

  abs    = fromR . abs . toR
  signum = fromR . signum . toR

  fromInteger i = (i :%: 1)