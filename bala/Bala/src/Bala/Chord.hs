{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Chord
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Chord representation 
--
--------------------------------------------------------------------------------

module Bala.Chord 
  (
  -- * Datatypes
    Chord
  , chordRoot 
  
  -- * Operations
  , makeChord
  , nthTone
  , chordThird
  , chordFifth
  , major
  , minor
  , diminished
  , augmented

  , insert      -- Ideally this wouldn't be exported
  , delete      --         - ditto -


  -- ** Transformers
  , noRoot
  , no3
  , no5
  , no7
  , no9
  , no11
  , no13
  , sus4

  , maj7
  , maj9
  , min7
  , min9
  , min11
  , aug9
  , aug11
  , aug13
  , dim7
  , dim9
  , dim11
  , dim13

  -- * Traids
  , TriadPattern
  , majorScaleTriads
  , naturalMinorTriads
  , harmonicMinorTriads
  , minorPentatonicBluesTriads

  ) where

import Bala.Interval
import Bala.Pitch

import Data.AffineSpace

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Monoid

data Chord = Chord { chordRoot :: Pitch, chordIntervals :: IntMap Int }
  deriving (Eq)


--------------------------------------------------------------------------------

instance Show Chord where
  showsPrec n ch@(Chord p _) = 
    shows p . showChar ':' . showsPrec n (map intervalName $ intervalContent ch)


instance PitchContent Chord  where 
  pitchContent ch@(Chord p _) = map (p .+^) $ intervalContent ch

instance IntervalContent Chord where
  intervalContent = map (uncurry makeInterval) . IM.toAscList . chordIntervals


--------------------------------------------------------------------------------


makeChord :: Pitch -> [Interval] -> Chord
makeChord p = Chord p . IM.fromList . map intervalPair



nthTone :: Int -> Chord -> Maybe Pitch
nthTone i (Chord rp imap) = 
  maybe Nothing (Just . (rp .+^) . makeInterval i) $ IM.lookup i imap

chordThird :: Chord -> Maybe Pitch
chordThird = nthTone 3


chordFifth :: Chord -> Maybe Pitch
chordFifth = nthTone 5


-- chordTone :: Int -> Chord -> Pitch
-- chordTone n ch = (pitchContent ch)!!n  

literalForm :: Pitch -> [Interval] -> Chord
literalForm p ivals = Chord p $ IM.fromList $ map intervalPair ivals

stepForm :: Pitch -> [Interval] -> Chord
stepForm p ivals = Chord p im where
  im = IM.fromList $ map intervalPair $ scanl mappend mempty ivals 


major :: Pitch -> Chord
major = literalForm `flip` [perfect1, major3, perfect5]

minor :: Pitch -> Chord
minor = literalForm `flip` [perfect1, minor3, perfect5]

diminished :: Pitch -> Chord
diminished = stepForm `flip` [minor3, minor3]

augmented :: Pitch -> Chord
augmented = stepForm `flip` [major3,major3]



immap :: (IntMap Int -> IntMap Int) -> Chord -> Chord
immap f (Chord p ivls) = Chord p (f ivls)

insert :: Interval -> Chord -> Chord
insert ival = immap (uncurry IM.insert $ intervalPair ival)

delete :: Int -> Chord -> Chord
delete i = immap (IM.delete i)

-- The 'Pachet' vocabulary - see Francois Pachet:
-- /An Object-Oriented Representation of Pitch-Classes, Intervals,
-- Scales and Chords: The basic MusES/

noRoot      :: Chord -> Chord
no3         :: Chord -> Chord
no5         :: Chord -> Chord
no7         :: Chord -> Chord
no9         :: Chord -> Chord
no11        :: Chord -> Chord
no13        :: Chord -> Chord
noRoot      = delete 1
no3         = delete 3
no5         = delete 5
no7         = delete 7
no9         = delete 9
no11        = delete 11
no13        = delete 13


sus4        :: Chord -> Chord
sus4        = insert perfect4 . delete 3


maj7        :: Chord -> Chord
maj9        :: Chord -> Chord
maj7        = insert major7
maj9        = insert major9 . maj7 

min7        :: Chord -> Chord
min9        :: Chord -> Chord
min11       :: Chord -> Chord
min7        = insert minor7
min9        = insert major9 . min7 
min11       = insert perfect11 . min9 


aug9        :: Chord -> Chord
aug11       :: Chord -> Chord
aug13       :: Chord -> Chord
aug9        = insert augmented9 . insert minor7
aug11       = insert augmented11 . aug9
aug13       = insert augmented13 . aug11

dim7        :: Chord -> Chord
dim9        :: Chord -> Chord
dim11       :: Chord -> Chord
dim13       :: Chord -> Chord
dim7        = insert diminished7
dim9        = insert diminished9 . insert minor7
dim11       = insert diminished11 . dim9
dim13       = insert diminished13 . dim11


-- maj13 :: Chord -> Chord
-- maj13 = insert major9 . maj9


--------------------------------------------------------------------------------
-- Generate the triads for a scale


type TriadPattern = [Pitch -> Chord]

majorScaleTriads :: TriadPattern
majorScaleTriads = 
    [major, minor, minor, major, major, minor, diminished, major]

naturalMinorTriads :: TriadPattern
naturalMinorTriads = 
    [minor, diminished, major, minor, minor, major, major, minor]

harmonicMinorTriads :: TriadPattern
harmonicMinorTriads = 
    [minor, diminished, major, minor, major, major, diminished, minor]

minorPentatonicBluesTriads :: TriadPattern
minorPentatonicBluesTriads = 
    [major, major, major, major, major, major]