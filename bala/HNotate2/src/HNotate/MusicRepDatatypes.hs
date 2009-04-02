{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.MusicRepDatatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Further data for representing music (other than pitch and duration) 
-- which have their own modules
--
--------------------------------------------------------------------------------

module HNotate.MusicRepDatatypes where

import HNotate.Duration
import HNotate.Pitch

import qualified Data.Map as Map
import Data.Ratio

--------------------------------------------------------------------------------
-- Music representation

  
-- For /universality/ meter is defined according to Abc's representation.
-- LilyPond will simply generate @TimeSig@ cases.
data Meter = TimeSig Integer Integer 
           -- | CommonTime is 4/4
           | CommonTime 
           -- | CutTime is 2/2
           | CutTime
  deriving (Eq,Show)
  
  
  
type MeterPattern = ([Integer],Duration)

meterPatternLength :: MeterPattern -> Duration
meterPatternLength (xs,d) = d * (%1) (fromIntegral $ sum xs)  

meterPatternDurations :: MeterPattern -> [Duration]
meterPatternDurations (xs,d) = map fn xs where
    fn i = d * (fromIntegral i % 1) 



data Key = Key PitchLabel Mode [PitchLabel]
  deriving (Eq,Show) 

  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Eq,Enum,Ord,Show)

newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int PitchLabel }
  deriving (Show)



labelSet :: [PitchLabel] -> LabelSet
labelSet = LabelSet . foldl fn Map.empty
  where fn m p = Map.insert (semitones p) p m
  
labelSetFind :: Pitch -> LabelSet -> Maybe Pitch
labelSetFind (Pitch l a o) (LabelSet m) = 
    maybe Nothing (fn o) (Map.lookup (semitones l + semitones a) m) 
  where
    fn ove (PitchLabel ltr atl) = Just $ Pitch ltr atl ove
    
    
    

barLength :: Meter -> Duration
barLength CommonTime    = 4 * 1%4
barLength CutTime       = 2 * 1%4
barLength (TimeSig n d) = n%d


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
naturalize :: LabelSet -> Pitch -> Pitch
naturalize lbls p = maybe p ((flip accidentalConst) Nat) (labelSetFind p lbls)
    


