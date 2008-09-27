
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.MusicRepDatatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Datatypes for representing music - principally these are the islands in
-- the LilyPond or Abc input files that control the note list generation
-- (e.g. key signature, meter)
--
--------------------------------------------------------------------------------

module HNotate.MusicRepDatatypes where

import HNotate.Duration
import HNotate.Pitch

import qualified Data.Map as Map
                      
--------------------------------------------------------------------------------
-- Music representation
type MeterPattern = ([Int],Duration)

four_four_of_eighth :: MeterPattern
four_four_of_eighth = ([4,4],eighth)


data Key = Key Pitch Mode
  deriving Show  

data Meter = TimeSig Int Int | CommonTime | CutTime
  deriving Show
  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving Show

newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int Pitch }
  deriving (Show)

labelSet :: [Pitch] -> LabelSet
labelSet = LabelSet . foldl fn Map.empty
  where fn m p = Map.insert (semitones p `mod` 12) p m
  
labelSetFind :: Pitch -> LabelSet -> Maybe Pitch
labelSetFind p@(Pitch _ _ o) (LabelSet m) = 
    maybe Nothing (fn o) (Map.lookup (semitones p `mod` 12) m) 
  where
    fn o p = Just $ octaveConst p o
    
    
c_major = Key middle_c Major  

four_four = TimeSig 4 4

meterToDouble :: Meter -> Double
meterToDouble (TimeSig n d) = (fromIntegral n) / (fromIntegral d)
meterToDouble CommonTime    = 4.0 / 4.0
meterToDouble CutTime       = 2.0 / 2.0

-- intervals?
labelSetOf :: Key -> LabelSet
labelSetOf (Key (Pitch C Nat _) Major)  = c_major_labels
labelSetOf (Key (Pitch A Nat _) Major)  = a_major_labels
labelSetOf _                            = c_major_labels


-- Enharmonically change the pitch name if it is in the label set
spell :: Pitch -> LabelSet -> Pitch
spell p@(Pitch _ _ o) lbls = 
    maybe p ((flip octaveConst) o) (labelSetFind p lbls)
  
-- Cancel the accidental if the pitch is found in the label set
-- This is the transformation needed for Abc: 
-- f# should be printed f in g major
naturalize :: Pitch -> LabelSet -> Pitch
naturalize p@(Pitch _ _ o) lbls = 
    maybe p ((flip accidentalConst) Nat) (labelSetFind p lbls)
    
    
--------------------------------------------------------------------------------
-- Label sets for common scales

c_major_labels :: LabelSet
c_major_labels = labelSet [c4,d4,e4,f4,g4,a4,b4]

a_major_labels :: LabelSet
a_major_labels = labelSet [a4, b4, cis5, d5, e5, fis5, gis5]

