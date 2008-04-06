
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

instance Arbitrary PitchLabel where
  arbitrary = PitchLabel <$> arbitrary <*> arbitrary
  coarbitrary = error "no coarbitrary for PitchLabel"
  
    
  
instance Arbitrary Accidental where
  arbitrary = elements $ 
    [Nat, Sharp 1, Sharp 2, Flat 1, Flat 2]
  coarbitrary = error "no coarbitrary for Accidental"  
  

-- PitchConversion
instance Arbitrary MidiPitch where
  arbitrary = M <$> arbitrary 
  coarbitrary = error "no coarbitrary for MidiPitch"

instance Arbitrary Hertz where
  arbitrary = Hz <$> arbitrary 
  coarbitrary = error "no coarbitrary for Hertz"

  
-- pitch class with an octave designation
instance Arbitrary OctavePC where
  arbitrary = OPC <$> arbitrary 
  coarbitrary = error "no coarbitrary for OctavePC"
    

instance Arbitrary OctaveRep where
  arbitrary = OR <$> arbitrary 
  coarbitrary = error "no coarbitrary for OctaveRep"  
    