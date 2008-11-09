--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Meter
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Meter (currently somewhat subsumed by Duration)
--
--------------------------------------------------------------------------------

module Bala.Base.Meter where

import Bala.Base.BaseExtra
import Bala.Base.Duration 

import Data.Ratio

-- * Meter fraction
-- | An alternative to Data.Rational which normalizes where possible
-- e.g. 4\/4 becomes 1\/1. For time signatures we don't want to normalize.
data MeterFraction = Int :/ Int
  deriving (Eq)


instance Enum MeterFraction where
  toEnum i = i :/ 1
  fromEnum (n :/ d) = n `div` d 

instance Ord MeterFraction where
  a `compare` b = toR a `compare` toR b  

instance Show MeterFraction where
  showsPrec _ (n :/ d) = shows n . showChar '/' . shows d

toR :: MeterFraction -> Ratio Int
toR (n :/ d) = n%d 

meterSize :: MeterFraction -> Duration
meterSize (n :/ d) = fromIntegral n * (1 % fromIntegral d)

pulse :: MeterFraction -> [Duration] 
pulse (n :/ d) = replicate n (1 % fromIntegral d)
  



              