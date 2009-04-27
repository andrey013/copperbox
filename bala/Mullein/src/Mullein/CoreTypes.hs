{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.CoreTypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
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

import Text.PrettyPrint.Leijen ( Doc )



--------------------------------------------------------------------------------
-- Musical representation


type MeterPattern = [Duration] 

type MetricalSpec = (Meter,MeterPattern)

  
data Key = Key PitchLabel Mode [PitchLabel]
  deriving (Eq,Show) 

  
data Mode = Major | Minor | Lydian | Ionian | Mixolydian
          | Dorian | Aeolian | Phrygian | Locrian 
  deriving (Bounded,Enum,Eq,Ord,Show) 
 
--------------------------------------------------------------------------------
-- 

data PartP e  = Part [PhraseP e]
  deriving (Eq,Show)

data PhraseP e = Phrase   (MotifP e)
               | Repeated (MotifP e)
               | FSRepeat (MotifP e) (MotifP e) (MotifP e)
  deriving (Eq,Show)

data MotifP e = Motif Key Meter [BarP e]
  deriving (Eq,Show)

data BarP e = Bar     (UnisonP e)
            | Overlay (UnisonP e) [UnisonP e]
  deriving (Eq,Show)


type Tied = Bool

data UnisonP e = Unison [BracketP e] Tied
  deriving (Eq,Show)


-- bracket together beamed notes to make a pulsation
data BracketP e = Singleton (ElementP e)
                | Bracket   [ElementP e]
  deriving (Eq,Show)


-- Pitch is the typical parameter for Element syntax tree.
-- However other variations so as LilyPond percussion can be handled.
-- With LilyPond percussion each note is a drum name rather than a pitch. 

data ElementP e = Note   e     Duration
                | Rest   Duration
                | Spacer Duration
                | Chord  [e]   Duration
                | GraceNotes [GraceNoteP e]
  deriving (Eq,Show)
        
type GraceNoteP e = (e, Duration)


instance Temporal (ElementP e) where 
  duration (Note _ d)             = d
  duration (Rest d)               = d
  duration (Spacer d)             = d
  duration (Chord _ d )           = d
  duration (GraceNotes _)         = 0
    
 
  
  swapDuration d (Note p _)       = Note p d
  swapDuration d (Rest _)         = Rest d
  swapDuration d (Spacer _)       = Spacer d
  swapDuration d (Chord se _)     = Chord se d
  swapDuration _ (GraceNotes se)  = GraceNotes se

instance Spacer (ElementP e) where
  spacer d     = Spacer d  

--------------------------------------------------------------------------------
-- Post processing and pretty printing scores

data OutputFragment a = MidtuneCmd Doc 
                      | BarOutput Doc
                      | Prefix a        -- e.g. "|:" 
                      | Suffix a        -- e.g. "|" or ":|"
  deriving (Show)

data BarDiv = RepStart | RepEnd | NRep Int | SglBar | DblBar 
  deriving (Eq,Show)


-- A Phantom type 
newtype P a = P { unP :: Doc }


infixl 5 +++
class Concat ctx ctx' where
  (+++)  :: P ctx -> P ctx' -> P ctx'

