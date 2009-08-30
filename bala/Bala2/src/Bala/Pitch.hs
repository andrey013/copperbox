{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Pitch
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch represention
--
--------------------------------------------------------------------------------

module Bala.Pitch  where

import Bala.Modulo

import Data.AdditiveGroup
import Data.AffineSpace


--------------------------------------------------------------------------------
-- Datatypes

data PitchLetter = C | D | E | F | G | A | B
  deriving (Enum,Eq,Ord,Show)

type Accidental = Int   -- 0 Nat, negative Flat, positive Sharp

type Octave = Int

data Pitch = Pitch { 
      pitchLetter :: PitchLetter, 
      accidental  :: Accidental,
      octave      :: Octave
    }
  deriving (Eq)



data Interval = Interval { arithmeticDistance :: Int, halfStepCount :: Int }
  deriving (Eq,Ord,Show)


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

instance Modulo7 PitchLetter where
  toZ7 = toZ7 . fromEnum
  fromZ7 = toEnum . fromZ7


instance Show Pitch where
  showsPrec _ (Pitch l a o) = shows l . fa a . shows o 
    where
      fa i | i > 0     = showString (replicate i '#')
           | i < 0     = showString (replicate (abs i) 'b')
           | otherwise = id

-- | Note affine operations on PitchLetters are patterned on the 
-- /retrograde/ counting used to calculate arithmetic distance.
-- For instance the arithmetic distance from C to C is 1 (not zero),
-- C to E is 3 (as the pitch letters C D and E are counted).
instance AffineSpace PitchLetter where
  type Diff PitchLetter = Int
  (.-.) a b = 1 + (mod7 $ fromEnum a - fromEnum b)
  (.+^) l i | i > 0     = toEnum (mod7 $ (i-1) + fromEnum l) 
            | i < 0     = toEnum (mod7 $ (i+1) + fromEnum l)
            | otherwise = error "PitchLetter .+^ 0 - undefined"


 
instance AdditiveGroup Interval where
  zeroV = Interval 0 0 
  (Interval ad hf) ^+^ (Interval ad' hf') = Interval (ad+ad') (hf+hf')
  negateV (Interval ad hf) = Interval (-ad) (-hf)

-- This will need some quickchecking...

instance AffineSpace Pitch where
  type Diff Pitch = Interval
  (.-.) p@(Pitch l _ _) p'@(Pitch l' _ _) = Interval ad sc
    where
      sc            = semitones p - semitones p'
      octave_steps  = 7*(sc `div` 12)
      ad0           = fromEnum $ l .-. l'
      ad            = if signum octave_steps < 0
                      then (negate ad0) + octave_steps
                      else ad0 + octave_steps
      
  (.+^) p@(Pitch l _ o) (Interval ad sc) = Pitch lbl acd ove
    where
      lbl       = l .+^ ad 
      acd       = accidental $ spell lbl (sc + semitones p)
      lbl_carry = if lbl < l && ad >0 then 1 else 0
      sc_carry  = if (sc>=0) then sc `div` 12 else 1 + sc `div` 12
      ove       = o + lbl_carry + sc_carry

spell :: PitchLetter -> Int -> Pitch
spell l semicount = Pitch l a o 
  where
    (o,i) = semicount `divMod` 12
    a     = i - semitones l


middleC :: Pitch 
middleC = Pitch C 0 5




