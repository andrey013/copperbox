
--------------------------------------------------------------------------------
-- |
-- Module      :  ArbitraryInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Arbitrary instances
--
--------------------------------------------------------------------------------


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


abs_signum_Prop x = abs x * signum x == x


--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Interval where
  arbitrary = fromInteger <$> choose (0,12)
  coarbitrary = error "no coarbitrary for Interval"
  
    
instance Arbitrary Pitch where
  arbitrary = fromInteger <$> choose (-12,72)
  coarbitrary = error "no coarbitrary for Pitch"

instance Arbitrary PitchLetter where
  arbitrary = elements [A, B, C, D, E, F, G]
  coarbitrary = error "no coarbitrary for PitchLetter"

instance Arbitrary PitchName where
  arbitrary = PitchName <$> arbitrary <*> arbitrary
  coarbitrary = error "no coarbitrary for PitchLabel"
  
    
  
instance Arbitrary Accidental where
  arbitrary = elements $ 
    [Nat, Sharp, Flat, DoubleSharp, DoubleFlat]
  coarbitrary = error "no coarbitrary for Accidental"  
  

-- PitchConversion
instance Arbitrary MidiPitch where
  arbitrary = midiPitch <$> arbitrary 
  coarbitrary = error "no coarbitrary for MidiPitch"

instance Arbitrary Hertz where
  arbitrary = hertz <$> arbitrary 
  coarbitrary = error "no coarbitrary for Hertz"

  
-- pitch class with an octave designation
instance Arbitrary OctavePitchClass where
  arbitrary = octavePitchClass <$> arbitrary 
  coarbitrary = error "no coarbitrary for OctavePitchClass"
    

instance Arbitrary OctaveFractional where
  arbitrary = octaveFractional <$> arbitrary 
  coarbitrary = error "no coarbitrary for OctaveFractional"  
    