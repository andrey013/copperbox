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

import Bala.Invert
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
  deriving Eq



data Interval = Interval { arithmeticDistance :: Int, halfStepCount :: Int }
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


instance Show Interval where
  showsPrec p (Interval ad sc) = showsPrec p (ad,sc)


instance Modulo7 PitchLetter where
  toZ7   = toZ7 . fromEnum
  fromZ7 = toEnum . fromZ7

instance Modulo12 Pitch where
  toZ12   = toZ12 . toSemitones
  fromZ12 = fromSemitones . fromZ12


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
  zeroV = Interval 1 0 
  (Interval ad sc) ^+^ (Interval ad' sc') = Interval (ad + f ad') (sc+sc')
    where f i | i > 0     = i-1
              | i < 0 = i+1
              | otherwise = error "^+^ ill-formed interval, ad should not be 0."

  negateV (Interval ad sc) = Interval (-ad) (-sc)

-- This will need some quickchecking...

instance AffineSpace Pitch where
  type Diff Pitch = Interval
  (.-.) p p' | p >= p'   = pdiff p p'
             | otherwise = negateV $ pdiff p' p   -- flip args
    where
      pdiff a@(Pitch la _ _) b@(Pitch lb _ _) = Interval ad sc where
        sc = toSemitones a - toSemitones b
        ad = (7*(sc `div` 12)) + (fromEnum $ la .-. lb)
      
  (.+^) p@(Pitch l _ o) (Interval ad sc) = Pitch lbl acd ove
    where
      lbl       = l .+^ ad 
      acd       = accidental $ spell lbl (sc + toSemitones p)
      lbl_carry = if lbl < l && ad >0 then 1 else 0
      sc_carry  = if (sc>=0) then sc `div` 12 else 1 + sc `div` 12
      ove       = o + lbl_carry + sc_carry

-- Note - cannot have an instance of AffineSpace for Pitch with:
--   type Diff Pitch = Semitones
--
-- AffineSpace is not a MPTC.


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

instance Invert Interval where
   -- Erk - this is correct only for simple intervals...
  invert (Interval ad sc) = Interval (9 - ad) (12 - sc)


data IntervalQuality = Diminished Int | Minor | Perfect | Major | Augmented Int
  deriving (Eq)

instance Show IntervalQuality where
  showsPrec _ (Diminished n) = showString $ replicate n 'd'
  showsPrec _ Minor          = showChar 'm'
  showsPrec _ Perfect        = showChar 'P'
  showsPrec _ Major          = showChar 'M'
  showsPrec _ (Augmented n)  = showString $ replicate n 'A'

intervalQuality :: Interval -> IntervalQuality
intervalQuality (Interval ad sc) = 
    either (dpa $ mod12 sc) (dmma $ mod12 sc) $ fn (amod7 ad)
  where
    fn 1 = Left   0
    fn 2 = Right (1,2)
    fn 3 = Right (3,4)
    fn 4 = Left   5 
    fn 5 = Left   7
    fn 6 = Right (8,9)
    fn 7 = Right (10,11)
    fn _ = error "intervalQuality - unreachable (0)"

    dpa s n | s > n     = Augmented (s-n)
            | s < n     = Diminished (n-s)
            | otherwise = Perfect

    dmma s (mn,mj) | s == mn  = Minor
                   | s == mj  = Major
                   | s <  mn  = Diminished (mn-s)
                   | s >  mj  = Augmented (s-mj)
                   | otherwise = error "intervalQuality - unreachable (1)"
                     

intervalName :: Interval -> String
intervalName ival@(Interval ad _) = show (intervalQuality ival) ++ show ad

-- amod7 [1-7]
amod7 :: Int -> Int
amod7 i = 1 + ((i-1) `mod` 7) 


-- Simple interval from compound interval
divSimple :: Interval -> (Int,Interval)
divSimple (Interval ad sc) = (d, Interval (amod7 ad) sc')
  where
    (d,sc') = sc `divMod` 12
