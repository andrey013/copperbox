{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbc.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Datatypes for ABC format in the final-tagless (Symantics) style of
-- Carette, Kiselyov, and Shan.
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.Datatypes (
  -- * Contexts
  CT_Field, CT_Element,

  -- * Information fields (3)
  Field, CField(..), 
  MidTuneField, CMidTuneField(..),

  -- ** M: meter (3.1.6)
  Meter, CMeter(..),

  -- ** Q: tempo (3.1.8)
  Tempo, CTempo(..),
  Length, CLength(..),

  -- ** K: key (3.1.14)
  Key, CKey(..),
  KeySpec, CKeySpec(..),
  KeyAccidental, CKeyAccidental(..),
  Mode, CMode(..),
  
  -- * The tune body (4)
  TuneBody, CTuneBody(..),
  AbcLine, CAbcLine(..),
  
  -- ** Pitch (4.1)
  PitchLetter(..),
  BaseNote, CBaseNote(..),
  Octave, COctave(..),

  -- ** Accidentals (4.2)
  Accidental, CAccidental(..),

  -- ** Note lengths (4.3)
  Duration, CDuration(..),

  -- ** Broken rhythm (4.4)
  BrokenRhythm, CBrokenRhythm(..),

  -- ** Rests (4.5)
  Rest, CRest(..),

  -- ** Repeat \/ bar symbols (4.8)
  RepeatMark, CRepeatMark(..),  

  -- ** Ties and slurs (4.11)  
  Tie, CTie(..),
  Slur, CSlur(..),
  
  -- ** Grace notes (4.12)
  GraceNotes, CGraceNotes(..),

  -- ** Duplets, triplets, quadruplets, etc. (4.13)
  NPlet, CNPlet(..),
  
  -- ** Decorations (4.13)
  Decoration, CDecoration(..),

  -- ** Chords and unisons (4.17)
  Chord, CChord(..), 

  -- * Multiple voices (7)
  -- ** Voice overlay (7.4)
  CVoiceOverlay(..)

  
  ) where

import Bala.Format.Base.SymBase
import Bala.Base.Meter

-- Contexts

-- | Fields
data CT_Field

  
-- | Elements in the tune body - notes, rest, slurs, barlines...
data CT_Element 


--------------------------------------------------------------------------------
-- * Information fields (3)


data Field
class CField repr where
  -- | @A field@ - area.
  area_field                :: String -> repr Field

  -- | @B field@ - book.
  book_field                :: String -> repr Field
  
  -- | @C field@ - composer name. 
  composer_field            :: String -> repr Field

  -- | @D field@ - discography.
  discography_field         :: String -> repr Field

  -- | @G field@ - group.
  group_field               :: String -> repr Field

  -- | @H field@ - history.
  history_field             :: [String] -> repr Field
  
  -- | @I field@ - information.
  information_field         :: String -> repr Field

  -- | @N field@ - notes.  
  notes_field               :: String -> repr Field

  -- | @O field@ - origin. 
  origin_field              :: String -> repr Field

  -- | @R field@ - rhythm. 
  rhythm_field              :: String -> repr Field

  -- | @S field@ - source.
  source_field              :: String -> repr Field

           
  -- | @X field@ - reference \/ tune number.
  number_field              :: Int -> repr Field
  
  -- | @Z field@ - transcriber notes.  
  transcriber_notes_field   :: String -> repr Field
  
instance ListContext CT_Field Field


data MidTuneField
class CMidTuneField repr where

  -- | @E field@ - elemskip.
  elemskip_field            :: String -> repr MidTuneField

  -- | @K field@ - key.
  key_field                 :: repr Key -> repr MidTuneField
  
  -- | @L field@ - default note length.
  default_note_length_field :: MeterFraction -> repr MidTuneField

  -- | @M field@ - meter.
  meter_field               :: repr Meter -> repr MidTuneField  
  
  -- | @P field@ - parts, simplified - parts are just represented as a string.
  parts_field               :: [Char] -> repr MidTuneField
  
  -- | @Q field@ - tempo.
  tempo_field               :: repr Tempo -> repr MidTuneField

  -- | @T field@ - title.
  title_field               :: String -> repr MidTuneField
  
  -- | @W field@ - words.  
  words_field               :: String -> repr MidTuneField

instance ListContext CT_Field MidTuneField

-- ** M: meter (3.1.6)
data Meter
class CMeter repr where
  meter      :: MeterFraction -> repr Meter
  commonTime :: repr Meter
  cutTime    :: repr Meter
  
  
-- ** Q: tempo (3.1.8)
data Tempo
class CTempo repr where
  tempo               :: Int -> repr Tempo
  ctempo              :: repr Length -> Int -> repr Tempo
  stempo              :: MeterFraction -> Int -> repr Tempo
  
data Length
class CLength repr where
  ilength             :: Int -> repr Length
  flength             :: MeterFraction -> repr Length


-- ** K: key (3.1.14)
data Key
class CKey repr where
  key                 :: repr KeySpec -> repr Key
  highlandNoKey       :: repr Key
  highlandMixolydian  :: repr Key


data KeySpec
class CKeySpec repr where
  keySpec :: repr BaseNote -> repr KeySpec
      
  
data KeyAccidental
class CKeyAccidental repr where
  keySharp  :: repr KeyAccidental
  keyFlat   :: repr KeyAccidental
  

data Mode
class CMode repr where
  mode :: String -> repr Mode


instance Attribute KeySpec Mode 


--------------------------------------------------------------------------------
-- * The tune body (4)

data TuneBody
class CTuneBody repr where
  tunebody :: repr AbcLine -> repr TuneBody

instance ListContext CT_Field TuneBody



data AbcLine
class CAbcLine repr where
  elements          :: repr a -> repr AbcLine
  midtuneField      :: repr MidTuneField -> repr AbcLine
  
  

-- ** Pitch (4.1)

-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data PitchLetter = C | D | E | F | G | A | B | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Enum,Ord,Show) 


data BaseNote
class CBaseNote repr where
  note          :: PitchLetter -> repr BaseNote

instance ListContext CT_Element BaseNote
  
data Octave
class COctave repr where
  octaveLow     :: Int -> repr Octave
  octaveHigh    :: Int -> repr Octave
    
instance Attribute BaseNote Octave




-- ** Accidentals (4.2)
data Accidental
class CAccidental repr where 
  natural       :: repr Accidental
  sharp         :: repr Accidental
  doubleSharp   :: repr Accidental
  flat          :: repr Accidental
  doubleFlat    :: repr Accidental



instance PrefixAttribute BaseNote Accidental

-- ** Note lengths (4.3)
data Duration
class CDuration repr where
  dur :: MeterFraction -> repr Duration

instance Attribute BaseNote Duration
instance Attribute Rest Duration


-- ** Broken rhythm (4.4)
data BrokenRhythm
class CBrokenRhythm repr where
  -- '>' left note dotted, right note halved
  dottedLeft    :: Int -> repr BrokenRhythm
    
  -- '<' left note halved, right note dotted 
  dottedRight   :: Int -> repr BrokenRhythm
  
-- ** Rests (4.5)
data Rest
class CRest repr where
  rest :: repr Rest

instance ListContext CT_Element Rest

-- ** Beams (4.7)

-- to do 

-- ** Repeat \/ bar symbols & First and second repeats (4.8 & 4.9)
        
data RepeatMark
class CRepeatMark repr where
  repeatMark :: String -> repr RepeatMark

instance ListContext CT_Element RepeatMark


-- ** Ties and slurs (4.11)  
data Tie
class CTie repr where
  tie         :: repr Tie
  
instance ListContext CT_Element Tie

data Slur
class CSlur repr where
  beginSlur :: repr Slur
  endSlur   :: repr Slur

instance ListContext CT_Element Slur

-- ** Grace notes (4.12)

-- | gracenotes are a prefix attibute of a note
data GraceNotes
class CGraceNotes repr where
  gracenotes :: [repr BaseNote] -> repr GraceNotes

-- Its simpler if we make gracenotes a glyph rather than a prefix attr of a note.  
-- instance PrefixAttribute BaseNote GraceNotes  
instance ListContext CT_Element GraceNotes

-- ** Duplets, triplets, quadruplets, etc. (4.13)
data NPlet
class CNPlet repr where
  nplet :: Int -> repr NPlet

instance ListContext CT_Element NPlet

-- ** Decorations (4.13)
data Decoration
class CDecoration repr where
    tilde       :: repr Decoration
    stacatto    :: repr Decoration
    downbow     :: repr Decoration
    upbow       :: repr Decoration

instance Attribute BaseNote Decoration



-- ** Chords and unisons (4.17)
data Chord
class CChord repr where
  chord :: [repr BaseNote] -> repr Chord

instance ListContext CT_Element Chord

-- * Multiple voices (7)
-- ** Voice overlay (7.4)
  
class CVoiceOverlay repr where
  (&\) :: repr a -> repr b -> repr a  
  
  
  


    


  
  