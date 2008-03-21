

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Chord
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord representations
-- |
--------------------------------------------------------------------------------


module Bala.Base.Chord where

import Bala.Base.PitchRep
import Bala.Base.PitchOps
import Bala.Base.Interval
import Bala.Base.PitchClass
import Bala.Base.BaseExtra

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Pos

-- a chord is like a pitch class set, but with possible intervals > 12
data Chord = Chord {
    chord_root       :: Pitch,
    semitone_interval_pattern :: [Int]
  }
  deriving (Eq,Show)


data RomanChord = RomanChord {
    root_alteration   :: Maybe Alteration, 
    scale_degree      :: Int,
    chord_quality     :: ChordQuality,
    chord_variation   :: Maybe Variation,    
    inversion_label   :: InversionLabel
  }
  deriving (Eq)

data Alteration = RSharp | RFlat
  deriving (Eq) 
  
data ChordQuality = RMajor | RMinor
  deriving (Eq)

data Variation = Dim | Aug
  deriving (Eq)
  
data InversionLabel = IRoot | IFirst | ISecond | IThird
  deriving (Eq)

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


                   
  
  
--------------------------------------------------------------------------------
-- operations
--------------------------------------------------------------------------------

buildChord :: Pitch -> IntervalPattern -> Chord
buildChord a (IntervalPattern xs) = Chord a (scanl shiftyPlus (fixedPitch a) xs)

-- these would be better as scale degrees (e.g. major triad 1-3-5), so 
-- we need a new parser
major_triad_pattern = IntervalPattern [5,4]
minor_triad_pattern = IntervalPattern [4,5]
diminished_pattern  = IntervalPattern [4,4]
augmented_pattern   = IntervalPattern [5,5]



pitches :: Chord -> [Pitch]
pitches (Chord root xs) = map (addSemi root) xs
  

-- | Build a triad from a roman chord (within a scale ?)
triad :: RomanChord -> () -> Chord
triad rc@(RomanChord {scale_degree=d}) () = buildChord (tstart d) (tip rc)
  where
    tstart :: Int -> Pitch
    tstart 1 = read "C4"
    tstart 2 = read "D4"
    tstart 3 = read "E4"
    tstart 4 = read "F4"
    tstart 5 = read "G4"
    tstart 6 = read "A4"
    tstart 7 = read "B4"

-- triad interval pattern
tip :: RomanChord -> IntervalPattern
tip (RomanChord {chord_quality=qual,chord_variation=var} ) = 
    alter (base_pattern qual) var 
  where 
    base_pattern RMajor = IntervalPattern [5,4]
    base_pattern RMinor = IntervalPattern [4,5]
    
    alter pat opt = maybe pat (variation pat) opt
    variation (IntervalPattern [a,b]) Dim = IntervalPattern [a, b - 1]
    variation (IntervalPattern [a,b]) Aug = IntervalPattern [a, b + 1]
    

-- would it be better to use a call to `read` here?
-- e.g.  scaleDegrees Maj' = read "1-3-5"
    
scaleDegrees :: ChordSuffix -> ScaleDegreePattern
scaleDegrees Maj' = ScaleDegreePattern [(1,Nat), (3,Nat), (5,Nat)]
scaleDegrees _    = undefined

    
        
