

module ArbitraryInstances where

import Bala

import Control.Applicative
import Control.Monad
import Test.QuickCheck

--------------------------------------------------------------------------------
-- QuickCheck helpers
--------------------------------------------------------------------------------

instance Applicative Gen where
  pure = return
  (<*>) = ap

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

  
instance Arbitrary PitchLetter where
  arbitrary = elements [A, B, C, D, E, F, G]
  coarbitrary = error "no coarbitrary for PitchLetter"
  
instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> choose (3,6) <*> pure 0  
  coarbitrary = error "no coarbitrary for Pitch"
  
instance Arbitrary Accidental where
  arbitrary = elements $ 
    [Nat, Sharp, SharpSharp, Flat, FlatFlat, Sharpi 3, Flati 3]
  coarbitrary = error "no coarbitrary for Accidental"  
  
  