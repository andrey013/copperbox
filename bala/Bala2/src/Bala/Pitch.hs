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

instance AffineSpace PitchLetter where
  type Diff PitchLetter = Int
  (.-.) a b = 1 + (fromZ7 $ toZ7 a - toZ7 b)
  (.+^) = undefined

instance AdditiveGroup Interval where
  zeroV = Interval 0 0 
  (Interval ad hf) ^+^ (Interval ad' hf') = Interval (ad+ad') (hf+hf')
  negateV (Interval ad hf) = Interval (-ad) (-hf)

-- This will need some quickchecking...

instance AffineSpace Pitch where
  type Diff Pitch = Interval
  (.-.) p@(Pitch l _ o) p'@(Pitch l' _ o') = Interval ad hc 
    where
      ad = 1 + (7*o + fromEnum l) - (7*o' + fromEnum l') 
      hc = semitones p - semitones p'

  (.+^) p@(Pitch l _ o) (Interval ad sc) = Pitch l' a' o' 
    where
      (carry,l') = fork (id,toEnum) $ ((ad-1) + fromEnum l) `divMod` 7
      a' = accidental $ spell l' (sc + semitones p)
      o' = o + carry + sc `div` 12

spell :: PitchLetter -> Int -> Pitch
spell l semicount = Pitch l a o 
  where
    (o,i) = semicount `divMod` 12
    a     = i - semitones l

 
fork :: (a->c, b->d) -> (a,b) -> (c,d)
fork (f,g) (a,b) = (f a, g b)

middleC :: Pitch 
middleC = Pitch C 0 5




