

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
-- A datatypes for representing pitch
-- |
--------------------------------------------------------------------------------

module Bala.Base.PitchRep where

import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- Cache the pitch letter & accidental for spelling only, and store semitones 
-- for calculations  
data Pitch = Pitch {
    label     :: PitchLabel,
    octave    :: Int,
    seimtones :: Int,
    cents     :: Int 
  }
  deriving (Eq,Read,Show)
  
data PitchLabel = PitchLabel PitchLetter Accidental
  deriving (Eq,Read,Show) 
  
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Read,Show)

data Accidental = Nat | Sharp Int | Flat Int
  deriving (Eq,Read,Show)
  
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

instance Enum Accidental where
  fromEnum Nat       = 0
  fromEnum (Sharp i) = i
  fromEnum (Flat i)  = negate i
  
  toEnum i | i  == 0   = Nat
           | i > 0     = Sharp i
           | otherwise = Flat $ negate i


-- pitch ops -- adding intervals etc need a naming scheme

class SemiDisplacement a where 
  addSemi :: a -> Int -> a
  subSemi :: a -> Int -> a
  sharp   :: a -> a
  flat    :: a -> a
  
  sharp a = a `addSemi` 1
  flat a  = a `subSemi` 1

class OctaveDisplacement a where   
  addOve  :: a -> Int -> a
  subOve  :: a -> Int -> a
 


spellWithSharps :: PitchLabel -> PitchLabel
spellWithSharps (PitchLabel l a)   = 
  toEnum $ semis l + semis a


instance SemiDisplacement PitchLabel where
  addSemi (PitchLabel l a) i = toEnum $ semis l + semis a + i
  subSemi (PitchLabel l a) i = toEnum $ semis l + semis a - i



instance SemiDisplacement Pitch where
  -- oc is "octave-carry" this isn't a very descriptive implementation
  -- and at some point should be done better
  (Pitch l o s c) `addSemi` i = 
    let od = i `div` 12
        -- (PitchLabel l' a') = toEnum $ semis l + semis a + (i `mod` 12)
        oc = 0 -- if (l' < l) then 1 else 0
    in Pitch l (o + od + oc) (s + i) c
  
  
  (Pitch l o s c) `subSemi` i = 
    let od = i `div` 12 
        -- (PitchLabel l' a') = toEnum $ semis l + semis a - (i `mod` 12)
        oc = 0 -- if (l' > l) then 1 else 0
    in Pitch l (o - (od + oc)) (s - i) c
  
  
    
instance OctaveDisplacement Pitch where   
  (Pitch l o s c) `addOve` i = Pitch l (o + i) s c
  (Pitch l o s c) `subOve` i = Pitch l (o - i) s c
  
centValue :: Pitch -> Int
centValue (Pitch l o s c) 
  = (octaveDisplacement o * 100) + (s * 100) + c

pitchValue :: Int -> Pitch
pitchValue i = error "pitchValue"

{-  
toCents (Pitch o l a c) = Cents $ 
  (octaveDisplacement o * 100) + ((semis l + semis a) * 100) + c

fromCents (Cents i) = undefined
-}


class Semitones a where semitones :: a -> Int

-- The semitone displacement upwards from C
instance Semitones PitchLetter where
  semitones C = 0
  semitones D = 2
  semitones E = 4
  semitones F = 5
  semitones G = 7
  semitones A = 9
  semitones B = 11

-- How many semitones is the pitch changed by its accidental? 
instance Semitones Accidental where
  semitones Nat        = 0
  semitones (Sharp i) = i
  semitones (Flat i)  = negate i

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a
  
    
buildPitch :: PitchLabel -> Int -> Int -> Pitch
buildPitch lbl o c = Pitch lbl o (semitones lbl) c
  where
    semis C = 0
    semis D = 2
    semis E = 4
    semis F = 5
    semis G = 7
    semis A = 9
    semis B = 11
    
    
class SemiToneCount a where semis :: a -> Int

-- The semitone displacement upwards from C
instance SemiToneCount PitchLetter where
  semis a        = semitones a

-- How many semitones is the pitch changed by its accidental? 
instance SemiToneCount Accidental where
  semis a        = semitones a


instance SemiToneCount Pitch where
  semis (Pitch l o s _) = s + (12 * o)
   
octaveDisplacement oct            = (oct - 4) * 12  
  


  
class EncodePitch a where 
  toPitch :: a -> Pitch  
  fromPitch :: Pitch -> a

instance EncodePitch Int where
  toPitch i = pitchValue i
  fromPitch p = centValue p 

sem :: Pitch -> Int -> Pitch
sem p i = error "sem" -- toPitch $ i + unCents (toCents p)
 
ove :: Pitch -> Int -> Pitch
ove p@(Pitch {octave=o'}) i = p {octave=o'+i} 

data ParsonsCode = PaR | PaU | PaD    
  deriving (Eq,Ord,Show)
  
contour :: [Pitch] -> [ParsonsCode]  
contour = zam diff
  where diff a b = case a `compare` b of
                    EQ -> PaR
                    LT -> PaU
                    GT -> PaD
  
data RefinedContour = ReR | ReUS | ReUL | ReDS | ReDL
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- Ord instances
--------------------------------------------------------------------------------

instance Ord Pitch where
  p1 `compare` p2 = error "Ord Pitch" -- (toCents p1) `compare` (toCents p2)






--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------

instance Enum PitchLabel where 
  fromEnum = fromEnumPitchLabel . spellWithSharps
    where
      fromEnumPitchLabel (PitchLabel C Nat)   = 0
      fromEnumPitchLabel (PitchLabel C (Sharp 1)) = 1
      fromEnumPitchLabel (PitchLabel D Nat)   = 2
      fromEnumPitchLabel (PitchLabel D (Sharp 1)) = 3
      fromEnumPitchLabel (PitchLabel E Nat)   = 4
      fromEnumPitchLabel (PitchLabel F Nat)   = 5
      fromEnumPitchLabel (PitchLabel F (Sharp 1)) = 6
      fromEnumPitchLabel (PitchLabel G Nat)   = 7
      fromEnumPitchLabel (PitchLabel G (Sharp 1)) = 8
      fromEnumPitchLabel (PitchLabel A Nat)   = 9
      fromEnumPitchLabel (PitchLabel A (Sharp 1)) = 10
      fromEnumPitchLabel (PitchLabel B Nat)   = 11
  
  toEnum 0   = PitchLabel C Nat
  toEnum 1   = PitchLabel C (Sharp 1)
  toEnum 2   = PitchLabel D Nat
  toEnum 3   = PitchLabel D (Sharp 1)
  toEnum 4   = PitchLabel E Nat
  toEnum 5   = PitchLabel F Nat
  toEnum 6   = PitchLabel F (Sharp 1)
  toEnum 7   = PitchLabel G Nat
  toEnum 8   = PitchLabel G (Sharp 1)
  toEnum 9   = PitchLabel A Nat
  toEnum 10  = PitchLabel A (Sharp 1)
  toEnum 11  = PitchLabel B Nat

  toEnum i  = toEnum $ i `mod` 12
  
  