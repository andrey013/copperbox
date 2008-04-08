

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.ChordSynonyms
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternative chord representations used for printing and parsing only
--
--------------------------------------------------------------------------------

module Bala.Base.ChordSynonyms where

import Bala.Base.PitchRep
import Bala.Base.Chord


-- | Roman chords are only for parsing roman notation.
data RomanChord = RomanChord {
    root_alteration   :: Maybe Alteration, 
    scale_degree      :: Int,
    chord_quality     :: ChordQuality,
    chord_variation   :: Maybe Variation,    
    inversion_label   :: Inversion
  }
  deriving (Eq)

data Alteration = RSharp | RFlat
  deriving (Eq) 
  
data ChordQuality = RMajor | RMinor
  deriving (Eq)

data Variation = Dim | Aug
  deriving (Eq)
  
-- | /Guitar/ style labelled chords.
data LabelledChord = LabelledChord {
    chord_pitch  :: PitchLetter,
    chord_suffix :: ChordSuffix
  }

data ChordSuffix 
  = -- Major   
    Maj' | Maj6 | Maj7 | Maj9 | Maj11 | Maj13 | MajAdd9 | Maj6_9
    -- Minor 
  | Min' | Min6 | Min7 | Min9 | Min11 | Min13 | MinAdd9 | Min6_9
  | MinMaj7 | MinMaj9 
    -- Dominant
  | Dom7 | Dom9 | Dom11 | Dom13  
  -- Diminished
  | Dim' | Dim7 | HalfDim7
  -- Augmented
  | Aug' | Aug7
  -- Suspended 
  | Sus2 | Sus4 | Sus7
  deriving (Eq)
  
  