

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
--
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

newtype ScaleDegreePattern = ScaleDegreePattern [(Int,Accidental)]

-- obsolete - change to IntervalStructure 
-- (actually rename IntervalStructure to IntervalPattern removing the 
-- definition below)
newtype IntervalPattern = IntervalPattern [Int]                   
  
  
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


instance Deco ScaleDegreePattern where
  deco = decoScaleDegreePattern
  
decoScaleDegreePattern = ScaleDegreePattern <$> sepBy1 scaleDegree whiteSpace 
  where scaleDegree = flip (,) <$> decoAccidental <*> int  
  
      
{-

-- new idea

data Alt = Nt | Sh Int | Fl Int | Inv Alt



data Chord = Chord { 
      chord_root  :: Pitch,
      chord_elems :: Map.Map Int Alt 
   }

n i p (Chord r m)  = Chord r (Map.insert i p m)

flatten = transp flatten'
  where flatten' Nt       = Fl 1
        flatten' (Sh 1)   = Nt
        flatten' (Sh i)   = Sh (i-1)
        flatten' (Fl i)   = Fl (i+1)
        flatten' (Inv a) = Inv $ flatten' a
        

  
instance Affi Chord where
  affi (Chord r m) = let xs = Map.toAscList m in
    affi r `sepS` hsepS (map (\(i,a) -> affi a . shows i) xs)


instance Affi Alt where
  affi Nt      = id
  affi (Sh i)  = showString (replicate i '#')
  affi (Fl i)  = showString (replicate i 'b')
  affi (Inv a) = showChar '^' . affi a

base = Chord c4 $ Map.fromAscList [(1,Nt),(3,Nt),(5,Nt)]


major = base

minor = flatten 3 major

add6 (Chord r m)   = Chord r $ Map.insert 6 Nt m

add9 (Chord r m)  = Chord r $ Map.insert 9 Nt m


transp fn i c@(Chord r m) = 
  maybe c (\a -> Chord r (Map.insert i (fn a) m)) (Map.lookup i m)

firstInversion = transp invert 1
secondInversion = firstInversion . transp invert 3


invert (Inv a) = Inv a
invert a       = Inv a

renderChord :: Chord -> [Pitch]
renderChord (Chord r m) = let xs = Map.toAscList m in
  map (step1 r) xs

-- problem addSemi loses the pitch spelling of arithmetic step
step1 :: Pitch -> (Int,Alt) -> Pitch
step1 p (i,a) = (p `arithmeticStep` i) `addSemi` (semitones a)


instance Semitones Alt where
  semitones Nt = 0
  semitones (Sh i) = i
  semitones (Fl i) = negate i
  semitones (Inv i) = 12 - semitones i







print_ans = afficherL . renderChord

-- Want we want in the `Chord` data structure
-- positional access - e.g. to flatten the third
-- access to the front
-- so use a finite map

-- Don't get hung up on order of ops, this should hold:
-- addNine $ addSix major == addSix $ addNine major

-}
        
