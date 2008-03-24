

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
    pitch_label   :: PitchLabel,
    octave        :: Int,
    seimtones     :: Int,
    cents         :: Int 
  }
  deriving (Eq,Read,Show)
  
data PitchLabel = PitchLabel {
    pitch_letter :: PitchLetter,
    accidental   :: Accidental
  }
  deriving (Eq,Read,Show) 
  
    
data PitchLetter = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Read,Show)

data Accidental = Nat | Sharp Int | Flat Int
  deriving (Eq,Read,Show)



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
  semitones Nat       = 0
  semitones (Sharp i) = i
  semitones (Flat i)  = negate i

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a

instance Semitones Pitch where
  semitones (Pitch l o s c) = 
    let (cc,_) = explode100 c in  12 * o + s + cc
  

--------------------------------------------------------------------------------
-- Enum instances
--------------------------------------------------------------------------------

instance Enum PitchLabel where 
  fromEnum pl = semitones pl

  
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
  
instance Enum Accidental where
  fromEnum Nat       = 0
  fromEnum (Sharp i) = i
  fromEnum (Flat i)  = negate i
  
  toEnum i | i  == 0   = Nat
           | i > 0     = Sharp i
           | otherwise = Flat $ negate i



--------------------------------------------------------------------------------
-- Ord instances
--------------------------------------------------------------------------------

semiCompare :: (Semitones a) => a -> a -> Ordering
semiCompare a b = semitones a `compare` semitones b


instance Ord Accidental where
  compare = semiCompare

instance Ord PitchLabel where
  compare = semiCompare
  
instance Ord Pitch where
  compare (Pitch _ o s c) (Pitch _ o' s' c') = a `compare` b
    where 
      (sc,cc)   = c  `divMod` 100
      (sc',cc') = c' `divMod` 100
      a         = (fromIntegral $ (12 * o) + s + sc) + ((fromIntegral cc) / 100.0)
      b         = (fromIntegral $ (12 * o') + s' + sc') + ((fromIntegral cc') / 100.0) 
    

--------------------------------------------------------------------------------
-- Num instances
--------------------------------------------------------------------------------

instance Num Accidental where
  a + b = toEnum $ fromEnum a + fromEnum b
                           
  a - b = toEnum $ fromEnum a - fromEnum b
    
  a * b = toEnum $ fromEnum a * fromEnum a

  abs (Flat i) = Sharp i  
  abs a        = a
  
  
  signum Nat       = Nat
  signum (Sharp _) = Sharp 1
  signum _         = Flat 1

    
  fromInteger = toEnum . fromIntegral   
  
instance Num Pitch where
  (Pitch l o s c) + (Pitch _ o' s' c') = Pitch l' (o + o' + co) s'' c''
    where (cs,c'') = explode100 (c + c')
          (co,s'') = explode12 (s + s' + cs)
          l' = toEnum $ s'' + fromEnum l


  (Pitch l o s c) - (Pitch _ o' s' c') = 
    let (cs,c'') = explode100 (c - c')
        (co,s'') = explode12 (s - s' + cs)
        l' = toEnum $ s'' - fromEnum l
    in Pitch l' (o - o' + co) s'' c''
  
  p * p' = 
    let semil     = fromIntegral $ semitones p
        semir     = fromIntegral $ semitones p'
        centl     = (fromIntegral $ cents p) / 100.0
        centr     = (fromIntegral $ cents p') / 100.0
        sp        = (semil + centl) * (semir + centr)
        (semis,c) = properFraction $ (semil + centl) * (semir + centr)
        (o,s)     = explode12 semis
    in Pitch (toEnum semis) o s (round (100 * c)) 
  
  abs p     = let (Pitch l o s _) = fromInteger $ fromIntegral $ abs $ semitones p
              in Pitch l o s (cents p)
              
  signum p  = case semitones p `compare` 0 of
                EQ -> Pitch (toEnum 0) 0 0 0
                GT -> Pitch (toEnum 0) 0 1 0
                LT -> Pitch (toEnum (-1)) (-1) 11 0
                
  
  -- note, this is different to midi - middle C here is 48 (in midi it is 60)
  fromInteger i = let i' = fromIntegral i; (o,s) = explode12  i'; l = toEnum i'
                  in Pitch l o s 0   
  