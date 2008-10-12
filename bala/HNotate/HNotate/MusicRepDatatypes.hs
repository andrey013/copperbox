
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


data PitchLabel = PitchLabel PitchLetter Accidental
  deriving (Eq,Show)

data Key = Key PitchLetter Accidental Mode
  deriving Show  

data Meter = TimeSig Int Int | CommonTime | CutTime
  deriving Show
  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Eq,Enum,Ord,Show)

newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int PitchLabel }
  deriving (Show)

instance Semitones PitchLabel where
  semitones (PitchLabel l a) = semitones l + semitones a

labelSet :: [PitchLabel] -> LabelSet
labelSet = LabelSet . foldl fn Map.empty
  where fn m p = Map.insert (semitones p) p m
  
labelSetFind :: Pitch -> LabelSet -> Maybe Pitch
labelSetFind p@(Pitch l a o) (LabelSet m) = 
    maybe Nothing (fn o) (Map.lookup (semitones l + semitones a) m) 
  where
    fn o (PitchLabel l a) = Just $ Pitch l a o
    
    
    
    


meterToDouble :: Meter -> Double
meterToDouble (TimeSig n d) = (fromIntegral n) / (fromIntegral d)
meterToDouble CommonTime    = 4.0 / 4.0
meterToDouble CutTime       = 2.0 / 2.0




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
    
-- How long does a meter patttern last? 
durationMP ::  MeterPattern -> Duration
durationMP (ds,d) = foldr (\e a -> a + (fromIntegral e) * d) duration_zero ds

