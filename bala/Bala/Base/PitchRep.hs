
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.PitchRep
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Base.PitchRep where

import Bala.Base.BaseExtra



--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | Note - there is redundancy between pitch_label and semitones, operations
-- on Pitch must take care to account for both.

 
data Pitch = Pitch {
    pch_label       :: PitchLabel,
    pch_octave      :: Int,
    pch_semitones   :: Int,
    pch_cents       :: Int 
  }
  deriving (Eq,Read,Show)

-- | Represent pitches independent of octave   
data PitchLabel = PitchLabel {
    pch_label_pitch_letter :: PitchLetter,
    pch_label_accidental   :: Accidental
  }
  deriving (Eq,Read,Show) 
  
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Ord,Read,Show)

data Accidental = Nat | Sharp | Flat | DoubleSharp | DoubleFlat
  deriving (Eq,Read,Show)

-- (Needs a smart constructor...)

-- | A /smart constructor/. It doesn't need semitones stating as it 
-- derives semitones from the 'PitchLabel'.
pitch :: PitchLabel -> Int -> Pitch
pitch lbl o = Pitch lbl o (semitoneCount lbl) 0
  where
    semis C = 0
    semis D = 2
    semis E = 4
    semis F = 5
    semis G = 7
    semis A = 9
    semis B = 11

withCents :: Pitch -> Int -> Pitch
withCents p i = let c = pch_cents p in p {pch_cents=c+i}


-- | Semitones is the basis for Pitch arithmetic
class SemitoneCount a where semitoneCount :: a -> Int

-- | Add and subtract semitones
class SemitoneExtension a where
  -- | Add a semitone to a /pitched value/.
  addSemi :: a -> Int -> a
  -- | Subtract a semitone from a /pitched value/.
  subSemi :: a -> Int -> a
  

-- | Add an octave to a /pitched value/.  
addOve  :: SemitoneExtension a => a -> Int -> a
addOve e = addSemi e . (12 *)

-- | Subtract an octave from a /pitched value/.
subOve  :: SemitoneExtension a => a -> Int -> a
subOve e = subSemi e . (12 *)



-- The semitone displacement upwards from C
instance SemitoneCount PitchLetter where
  semitoneCount l = fromEnum (PitchLabel l Nat)

-- How many semitones is the pitch changed by its accidental? 
instance SemitoneCount Accidental where
  semitoneCount a       = fromEnum a

instance SemitoneCount PitchLabel where
  semitoneCount (PitchLabel l a) = semitoneCount l + semitoneCount a

instance SemitoneCount Pitch where
  semitoneCount (Pitch l o s c) = 
    let (cc,_) = explode100 c in  12 * o + s + cc
    
    
--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------

instance Enum PitchLetter where 
  fromEnum C = 0
  fromEnum D = 1
  fromEnum E = 2
  fromEnum F = 3
  fromEnum G = 4
  fromEnum A = 5
  fromEnum B = 6
  
  toEnum 0   = C
  toEnum 1   = D
  toEnum 2   = E
  toEnum 3   = F
  toEnum 4   = G
  toEnum 5   = A
  toEnum 6   = B

  toEnum i  = toEnum $ i `mod` 7

instance Enum Accidental where
  fromEnum Nat          = 0
  fromEnum Sharp        = 1
  fromEnum DoubleSharp  = 2
  fromEnum Flat         = (-1)
  fromEnum DoubleFlat   = (-2)
  
  toEnum 0    = Nat
  toEnum 1    = Sharp
  toEnum 2    = DoubleSharp
  toEnum (-1) = Flat
  toEnum (-2) = DoubleFlat
  
instance Enum PitchLabel where 
  fromEnum (PitchLabel l a) = fn l + fromEnum a
    where fn C = 0
          fn D = 2
          fn E = 4
          fn F = 5
          fn G = 7
          fn A = 9
          fn B = 11

  
  toEnum 0   = PitchLabel C Nat
  toEnum 1   = PitchLabel C Sharp
  toEnum 2   = PitchLabel D Nat
  toEnum 3   = PitchLabel D Sharp
  toEnum 4   = PitchLabel E Nat
  toEnum 5   = PitchLabel F Nat
  toEnum 6   = PitchLabel F Sharp
  toEnum 7   = PitchLabel G Nat
  toEnum 8   = PitchLabel G Sharp
  toEnum 9   = PitchLabel A Nat
  toEnum 10  = PitchLabel A Sharp
  toEnum 11  = PitchLabel B Nat

  toEnum i  = toEnum $ i `mod` 12
  




--------------------------------------------------------------------------------
-- Ord instances
--------------------------------------------------------------------------------


instance Ord Accidental where
  compare a a' = fromEnum a `compare` fromEnum a' 

instance Ord PitchLabel where
  compare a a' = fromEnum a `compare` fromEnum a' 
  
instance Ord Pitch where
  compare (Pitch _ o s c) (Pitch _ o' s' c') = a `compare` b
    where 
      (sc,cc)   = explode100 c
      (sc',cc') = explode100 c'
      a         = octaveToCents o + semitonesToCents (s + sc) + cc
      b         = octaveToCents o' + semitonesToCents (s' + sc') + cc'
    
semitonesToCents = (1000 *)
octaveToCents   = (12 * 1000 *)
