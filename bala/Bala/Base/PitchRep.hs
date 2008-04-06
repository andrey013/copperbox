
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

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | Note - there is redundancy between pitch_label and semitones, operations
-- on Pitch must take care to account for both. 
data Pitch = Pitch {
    pitch_label   :: PitchLabel,
    octave        :: Int,
    seimtones     :: Int,
    cents         :: Int 
  }
  deriving (Eq,Read,Show)

-- | Represent pitches independent of octave   
data PitchLabel = PitchLabel {
    pitch_letter :: PitchLetter,
    accidental   :: Accidental
  }
  deriving (Eq,Read,Show) 
  
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Ord,Read,Show)

data Accidental = Nat | Sharp Int | Flat Int
  deriving (Eq,Read,Show)




  

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
  fromEnum Nat       = 0
  fromEnum (Sharp i) = i
  fromEnum (Flat i)  = negate i
  
  toEnum i | i  == 0   = Nat
           | i > 0     = Sharp i
           | otherwise = Flat $ negate i  
  
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
      (sc,cc)   = c  `divMod` 100
      (sc',cc') = c' `divMod` 100
      a         = (fromIntegral $ (12 * o) + s + sc) + ((fromIntegral cc) / 100.0)
      b         = (fromIntegral $ (12 * o') + s' + sc') + ((fromIntegral cc') / 100.0) 
    

