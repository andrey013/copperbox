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
  
  