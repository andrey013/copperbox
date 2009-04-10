{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.CoreTypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Fundamental datatypes
--
--------------------------------------------------------------------------------

module Mullein.CoreTypes where

import Mullein.Duration
import Mullein.Pitch

import qualified Data.Map as Map
import Data.Ratio
import qualified Data.Sequence as S



--------------------------------------------------------------------------------
-- Note lists

-- The Element datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 
data Element = 
      Note 
        { note_pitch          :: Pitch
        , elt_duration        :: Duration
        }                  
    | Rest  
        { elt_duration        :: Duration }
    | Spacer  
        { elt_duration        :: Duration }
    | Chord 
        { chord_elements      :: [Pitch] 
        , rhythmic_value      :: Duration
        }          
    | GraceNotes 
        { grace_elements      :: [GraceNote] }                              
    | Nplet 
        { nplet_multipier     :: Int
        , unit_duration       :: Duration
        , nplet_elements      :: [Pitch] 
        }                   
  deriving (Show) 

type GraceNote = (Pitch,Duration)

type NoteList = S.Seq Element


instance Temporal Element where 
  duration (Note _ d)             = d
  duration (Rest d)               = d
  duration (Spacer d)             = d
  duration (Chord _ d )           = d
  duration (GraceNotes _)         = duration_zero
  duration (Nplet i d _)          = (fromIntegral i % 1) * d 
    
 
  
  swapDuration d (Note p _)       = Note p d
  swapDuration d (Rest _)         = Rest d
  swapDuration d (Spacer _)       = Spacer d
  swapDuration d (Chord se _)     = Chord se d
  swapDuration _ (GraceNotes se)  = GraceNotes se
  swapDuration d (Nplet i _ se)   = Nplet i (reunit d i se) se where 
      reunit :: Duration -> Int -> [a] -> Duration
      reunit tot n xs = tot * (makeDuration l n) * (makeDuration 1 l) where
          l = length xs 
                  
                  
                  
instance Spacer Element where
  spacer d = Spacer d



--------------------------------------------------------------------------------
-- Musical representation

-- For /universality/ meter is defined according to Abc's representation.
-- LilyPond will simply generate @TimeSig@ cases.
data Meter = TimeSig Integer Integer 
           -- | CommonTime is 4/4
           | CommonTime 
           -- | CutTime is 2/2
           | CutTime
  deriving (Eq,Show)

type MeterPattern = [Duration] 

type MetricalSpec = (Meter,MeterPattern)

  
data Key = Key PitchLabel Mode [PitchLabel]
  deriving (Eq,Show) 

  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Eq,Enum,Ord,Show) 


newtype LabelSet = LabelSet { getLabelSet :: Map.Map Int PitchLabel }
  deriving (Show)
           