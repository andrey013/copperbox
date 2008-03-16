

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

-- PitchRep
instance Arbitrary PitchLetter where
  arbitrary = elements [A, B, C, D, E, F, G]
  coarbitrary = error "no coarbitrary for PitchLetter"

instance Arbitrary PitchLabel where
  arbitrary = PitchLabel <$> arbitrary <*> arbitrary
  coarbitrary = error "no coarbitrary for PitchLabel"
  
    
instance Arbitrary Pitch where
  arbitrary = Pitch <$> arbitrary <*> arbitrary <*> choose (3,6) <*> pure 0  
  coarbitrary = error "no coarbitrary for Pitch"
  
instance Arbitrary Accidental where
  arbitrary = elements $ 
    [Nat, Sharpi 1, Sharpi 2, Flati 1, Flati 2]
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
    