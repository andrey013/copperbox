{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Pitch
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

module Bala.Pitch  where


import Data.AdditiveGroup
import Data.AffineSpace


--------------------------------------------------------------------------------
-- Datatypes

data PitchLetter = C | D | E | F | G | A | B
  deriving (Enum,Eq,Ord,Show)

type Accidental = Int   -- 0 Nat, negative Flat, positive Sharp

type Octave = Int

data Pitch = Pitch PitchLetter Accidental Octave
  deriving (Eq,Show)

data Interval = Interval { arithmeticDistance :: Int, halfStepCount :: Int }
  deriving (Eq,Ord,Show)



accidental :: Pitch -> Accidental
accidental (Pitch _ a _) = a

--------------------------------------------------------------------------------
-- Type classes 

class Semitones a where
  semitones :: a -> Int

instance Semitones Pitch where
  semitones (Pitch l a o) = semitones l + a + (12 * o)

instance Semitones PitchLetter where    
  semitones C = 0
  semitones D = 2
  semitones E = 4
  semitones F = 5
  semitones G = 7
  semitones A = 9
  semitones B = 11
  
--------------------------------------------------------------------------------
-- Instances

instance AdditiveGroup Interval where
  zeroV = Interval 0 0 
  (Interval ad hf) ^+^ (Interval ad' hf') = Interval (ad+ad') (hf+hf')
  negateV (Interval ad hf) = Interval (-ad) (-hf)

-- This will need some quickchecking...

instance AffineSpace Pitch where
  type Diff Pitch = Interval
  (.-.) p@(Pitch l _ o) p'@(Pitch l' _ o') = Interval ad hc 
    where
      ad = (7*o + fromEnum l) - (7*o' + fromEnum l') 
      hc = semitones p - semitones p'
  (.+^) p@(Pitch l _ o) (Interval ad hc) = Pitch l' a' o' 
    where
      l' = toEnum $ (ad + fromEnum l) `mod` 7
      a' = accidental $ spell l' (hc + semitones p)
      o' = o + hc `div` 12

spell :: PitchLetter -> Int -> Pitch
spell l semicount = Pitch l a o 
  where
    (o,i) = semicount `divMod` 12
    a     = i - semitones l

 


middleC :: Pitch 
middleC = Pitch C 0 5

