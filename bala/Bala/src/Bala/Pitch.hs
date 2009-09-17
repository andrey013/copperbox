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

import Bala.Interval
import Bala.Modulo

import Data.AffineSpace

import Test.QuickCheck

--------------------------------------------------------------------------------
-- Datatypes

data PitchLetter = C | D | E | F | G | A | B
  deriving (Bounded,Enum,Eq,Ord,Show)

type Accidental = Int   -- 0 Nat, negative Flat, positive Sharp

type Octave = Int

data Pitch = Pitch { 
      pitchLetter :: PitchLetter, 
      accidental  :: Accidental,
      octave      :: Octave
    }
  deriving Eq




--------------------------------------------------------------------------------
-- Type classes 

class Semitones a where
  toSemitones   :: a -> Int
  fromSemitones :: Int -> a

instance Semitones Pitch where
  toSemitones (Pitch l a o) = toSemitones l + a + (12 * o)
  fromSemitones i = Pitch lbl (m - toSemitones lbl) o 
    where
      (o,m) = i `divMod` 12
      lbl   = fromSemitones m
    

instance Semitones PitchLetter where    
  toSemitones C = 0
  toSemitones D = 2
  toSemitones E = 4
  toSemitones F = 5
  toSemitones G = 7
  toSemitones A = 9
  toSemitones B = 11
  
  -- fromSemitones favours sharps and a pitch created with
  -- @fromSemitones@ may need respelling afterwards.
  fromSemitones = fn . mod12 where
    fn 0  = C
    fn 1  = C
    fn 2  = D
    fn 3  = D
    fn 4  = E
    fn 5  = F
    fn 6  = F
    fn 7  = G
    fn 8  = G 
    fn 9  = A
    fn 10 = A
    fn _  = B



  
--------------------------------------------------------------------------------
-- Instances



instance Show Pitch where
  showsPrec _ (Pitch l a o) = shows l . fa a . shows o 
    where
      fa i | i > 0     = showString (replicate i '#')
           | i < 0     = showString (replicate (abs i) 'b')
           | otherwise = id

instance Ord Pitch where
  compare a b = compare (toSemitones a) (toSemitones b)


instance Arbitrary PitchLetter where
  arbitrary = elements [C,D,E,F,G,A,B]
  coarbitrary = variant . fromEnum


-- Note - arbitrary does note generate a very /wide/ set of 
-- candidates (i.e. notes with unusual accidentals). 
instance Arbitrary Pitch where
  arbitrary = fmap fromSemitones $ choose (12,107)
  coarbitrary (Pitch l a o) = coarbitrary l . coarbitrary a . coarbitrary o




instance Modulo7 PitchLetter where
  toZ7   = toZ7 . fromEnum
  fromZ7 = toEnum . fromZ7

instance Modulo12 Pitch where
  toZ12   = toZ12 . toSemitones
  fromZ12 = fromSemitones . fromZ12


-- Intervals are not considered to be signed!
-- In fact the arithmetic distance and semitone count should 
-- always be positive.
-- This means the affine operations will not be associative. (sure?)


instance AffineSpace Pitch where
  type Diff Pitch = Interval
  (.-.) p p' | p >= p'   = pdiff p p'
             | otherwise = pdiff p' p   -- flip args
    where
      pdiff lo@(Pitch llo _ _) hi@(Pitch lhi _ _) = makeInterval ad sc where
        sc = toSemitones hi - toSemitones lo
        ad = addOves (sc `div` 12) (llo `upTo` lhi)

  -- ad and sc /should/ be positive!    
  (.+^) p@(Pitch l _ o) (Interval ad sc) = Pitch lbl acd ove
    where
      lbl       = toEnum $ mod7 $ (fromEnum l) + (ad - 1) 
      acd       = accidental $ spell lbl (sc + toSemitones p)
      lbl_carry = if lbl < l then 1 else 0
      ove       = o + lbl_carry + (sc `div` 12)


-- TODO.
-- This isn't a good place for dealing with arithmetic distances 
-- and their funny counting. Even though arithmetic distances are
-- no longer a distinct type the code should be localized in 
-- Bala.Interval.
addOves :: Int -> Int -> Int
addOves i = iter i (+(8-1))   -- check
  where 
    iter n f a | n <= 0    = a
               | otherwise = iter (n-1) f (f a)




spell :: PitchLetter -> Int -> Pitch
spell l semicount = Pitch l a o 
  where
    (o,i) = semicount `divMod` 12
    a     = i - toSemitones l


middleC :: Pitch 
middleC = Pitch C 0 5



-- | Increment the semitone count.
addSemitones :: Semitones a => a -> Int -> a
addSemitones a i = fromSemitones (i + toSemitones a)



upTo :: PitchLetter -> PitchLetter -> Int
upTo a b | a == b    = 1
         | a >  b    = 1 + (7 + fromEnum b) - fromEnum a
         | otherwise = 1 + (fromEnum b) - fromEnum a

downTo :: PitchLetter -> PitchLetter -> Int
downTo a b | a == b    = 1
           | a >  b    = 1 + (fromEnum a) - fromEnum b
           | otherwise = 1 + (7 + fromEnum a) - fromEnum b
