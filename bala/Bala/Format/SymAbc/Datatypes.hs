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

module Bala.Format.SymAbc.Datatypes  where

import Bala.Format.Base.SymBase
 
    
data CT_Field

  
-- | notes, rest, slurs, barlines...
data CT_Element 

data CT_Line






--------------------------------------------------------------------------------
-- * Fields

data Field
data MidTuneField

instance ListContext CT_Field Field
instance ListContext CT_Field MidTuneField


-- | @X field@ - reference \/ tune number.
class CFieldNumber repr where
  num_                :: Int -> repr Field

-- | @T field@ - title. 
class CFieldTitle repr where
  title_              :: String -> repr MidTuneField

-- | @A field@ - area.
class CFieldArea repr where
  area_               :: String -> repr Field

-- | @B field@ - book.
class CFieldBook repr where  
  book_               :: String -> repr Field

-- | @C field@ - composer name.
class CFieldComposer repr where  
  composer_           :: String -> repr Field

-- | @D field@ - discography.
class CFieldDiscography repr where  
  discography_        :: String -> repr Field

-- | @E field@ - elemskip.
class CFieldElemskip repr where
  elemskip_           :: String -> repr MidTuneField

-- | @G field@ - group.
class CFieldGroup repr where  
  group_              :: String -> repr Field

-- | @I field@ - information.
class CFieldInformation repr where
  information_        :: String -> repr Field

-- | @N field@ - notes.
class CFieldNotes repr where  
  notes_              :: String -> repr Field

-- | @O field@ - origin.
class CFieldOrigin repr where  
  origin_             :: String -> repr Field

-- | @R field@ - rhythm.
class CFieldRhythm repr where  
  rhythm_             :: String -> repr Field

-- | @S field@ - source.
class CFieldSource repr where  
  source_             :: String -> repr Field

-- | @W field@ - words.
class CFieldWords repr where    
  words_              :: String -> repr MidTuneField

-- | @Z field@ - transcriber notes.  
class CFieldTranscrNotes repr where
  transcrNotes_   :: String -> repr Field

-- | @H field@ - history.
class CFieldHistory repr where
  history_      :: [String] -> repr Field

-- | @K field@ - key.
class CFieldKey repr where
  key_          :: repr Key -> repr MidTuneField
  
-- | @L field@ - default note length.
class CFieldDefaultNoteLength repr where
  defaultNoteLength_    :: MeterFraction -> repr MidTuneField


-- | @P field@ - parts, simplified - parts are just represented as a string.
class CFieldParts repr where 
  parts_        :: [Char] -> repr MidTuneField
  
-- | @Q field@ - tempo.
class CFieldTempo repr where
  tempo_        :: repr Tempo -> repr MidTuneField

-- | @M field@ - meter.
class CFieldMeter repr where
  meter_        :: repr Meter -> repr MidTuneField  

data AbcMusic
class CAbcMusic repr where
  abcmusic :: repr AbcLine -> repr AbcMusic

instance ListContext CT_Field AbcMusic


data AbcLine
class CAbcLine repr where
  elements          :: repr a -> repr AbcLine
  midtuneField      :: repr MidTuneField -> repr AbcLine
    
  
data Tempo
class CTempo repr where
  tempo               :: Int -> repr Tempo
  ctempo              :: repr Length -> Int -> repr Tempo
  stempo              :: MeterFraction -> Int -> repr Tempo
  
data Length
class CLength repr where
  ilength             :: Int -> repr Length
  flength             :: MeterFraction -> repr Length

  
  
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



data Meter
class CMeter repr where
  meter      :: MeterFraction -> repr Meter
  commonTime :: repr Meter
  cutTime    :: repr Meter

    

data Duration
class CDuration repr where
  dur :: Int -> repr Duration

instance Attribute BaseNote Duration
instance Attribute Rest Duration



data Rest
class CRest repr where
  rest :: repr Rest

instance ListContext CT_Element Rest


data Octave
class COctave repr where
  octaveLow     :: Int -> repr Octave
  octaveHigh    :: Int -> repr Octave
    
instance Attribute BaseNote Octave


data Accidental
class CAccidental repr where 
  natural       :: repr Accidental
  sharp         :: repr Accidental
  doubleSharp   :: repr Accidental
  flat          :: repr Accidental
  doubleFlat    :: repr Accidental



instance Attribute BaseNote Accidental


-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data PitchLetter = C | D | E | F | G | A | B | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Show) 

data BaseNote
class CBaseNote repr where
  note          :: PitchLetter -> repr BaseNote

instance ListContext CT_Element BaseNote




data BrokenRhythm
class CBrokenRhythm repr where
  -- '>' left note dotted, right note halved
  dottedLeft    :: Int -> repr BrokenRhythm
    
  -- '<' left note halved, right note dotted 
  dottedRight   :: Int -> repr BrokenRhythm
  

  
data Tie
class CTie repr where
  tie         :: repr Tie

  
      
data Grace
class CGrace repr where
    tilde       :: repr Grace
    stacatto    :: repr Grace
    downbow     :: repr Grace
    upbow       :: repr Grace

instance Attribute BaseNote Grace
    
data NPlet
class CNPlet repr where
  nplet :: Int -> repr NPlet

instance ListContext CT_Element NPlet
     
        
data RepeatMark
class CRepeatMark repr where
  repeatMark :: String -> repr RepeatMark

instance ListContext CT_Element RepeatMark

 

data Slur
class CSlur repr where
  beginSlur :: repr Slur
  endSlur   :: repr Slur

instance ListContext CT_Element Slur

-- | gracenotes are a prefix attibute of a note
data GraceNotes
class CGraceNotes repr where
  gracenotes :: [repr BaseNote] -> repr GraceNotes

instance Attribute BaseNote GraceNotes  

data MultiNote
class CMultiNote repr where
  multinote :: [repr BaseNote] -> repr MultiNote

  
data TexCommand
class CTexComamnd repr where
  texCommand :: String -> repr TexCommand
  
  