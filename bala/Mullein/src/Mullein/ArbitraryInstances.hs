{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.ArbitraryInstances
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Arbitrary instances for QuickCheck testing.
--
--------------------------------------------------------------------------------



module Mullein.ArbitraryInstances where

import Mullein.Core
import Mullein.Pitch


import Control.Applicative
import Control.Monad ( ap )
import Test.QuickCheck

instance Applicative Gen where
  pure  = return
  (<*>) = ap

boundedEnums :: (Enum a, Bounded a) => [a]
boundedEnums = enumFromTo minBound maxBound

instance Arbitrary PitchLetter where
  arbitrary = elements $ boundedEnums
  coarbitrary = variant . fromEnum


instance Arbitrary Accidental where
  arbitrary = elements $ boundedEnums
  coarbitrary = variant . fromEnum

instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> choose (0,8)
  coarbitrary (Pitch l a o) = coarbitrary l . coarbitrary a . coarbitrary o

instance Arbitrary PitchLabel where
  arbitrary = PitchLabel <$> arbitrary <*> arbitrary
  coarbitrary (PitchLabel l a) = coarbitrary l . coarbitrary a  

instance Arbitrary Mode where
  arbitrary = elements $ boundedEnums
  coarbitrary = variant . fromEnum

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary <*> arbitrary
  coarbitrary (Key l m) = coarbitrary l . coarbitrary m


