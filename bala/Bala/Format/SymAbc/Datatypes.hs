{-# LANGUAGE EmptyDataDecls #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbc.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations
--
-- Datatypes for ABC format in the final-tagless (Symantics) style of
-- Carette, Kiselyov, and Shan.
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.Datatypes  where

import Bala.Format.Base.SymBase
 
    
data Ctx_Field
data Ctx_Line
 
  
-- | notes, rest, slurs, barlines...
data Ctx_Element 





data Field ctx
data MidTuneField ctx 

-- * Fields
-- | @X field@ - reference \/ tune number.
class SymFieldNumber repr where
  num_                :: Int -> repr (Field Ctx_Field)

-- | @T field@ - title. 
class SymFieldTitle repr where
  title_              :: String -> repr (MidTuneField Ctx_Field)

-- | @A field@ - area.
class SymFieldArea repr where
  area_               :: String -> repr (Field Ctx_Field)

-- | @B field@ - book.
class SymFieldBook repr where  
  book_               :: String -> repr (Field Ctx_Field)

-- | @C field@ - composer name.
class SymFieldComposer repr where  
  composer_           :: String -> repr (Field Ctx_Field)

-- | @D field@ - discography.
class SymFieldDiscography repr where  
  discography_        :: String -> repr (Field Ctx_Field)

-- | @E field@ - elemskip.
class SymFieldElemskip repr where
  elemskip_           :: String -> repr (MidTuneField Ctx_Field)

-- | @G field@ - group.
class SymFieldGroup repr where  
  group_              :: String -> repr (Field Ctx_Field)

-- | @I field@ - information.
class SymFieldInformation repr where
  information_        :: String -> repr (Field Ctx_Field)

-- | @N field@ - notes.
class SymFieldNotes repr where  
  notes_              :: String -> repr (Field Ctx_Field)

-- | @O field@ - origin.
class SymFieldOrigin repr where  
  origin_             :: String -> repr (Field Ctx_Field)

-- | @R field@ - rhythm.
class SymFieldRhythm repr where  
  rhythm_             :: String -> repr (Field Ctx_Field)

-- | @S field@ - source.
class SymFieldSource repr where  
  source_             :: String -> repr (Field Ctx_Field)

-- | @W field@ - words.
class SymFieldWords repr where    
  words_              :: String -> repr (MidTuneField Ctx_Field)

-- | @Z field@ - transcriber notes.  
class SymFieldTranscrNotes repr where
  transcrNotes_   :: String -> repr (Field Ctx_Field)

-- | @H field@ - history.
class SymFieldHistory repr where
  history_      :: [String] -> repr (Field Ctx_Field)

-- | @K field@ - key.
class SymFieldKey repr where
  key_          :: repr (Key ctx) -> repr (MidTuneField Ctx_Field)
  
-- | @L field@ - default note length.
class SymFieldDefaultNoteLength repr where
  defaultNoteLength_    :: MeterFraction -> repr (MidTuneField Ctx_Field)


-- | @P field@ - parts, simplified - parts are just represented as a string.
class SymFieldParts repr where 
  parts_        :: [Char] -> repr (MidTuneField Ctx_Field)
  
-- | @Q field@ - tempo.
class SymFieldTempo repr where
  tempo_        :: repr (Tempo ctx) -> repr (MidTuneField Ctx_Field)

-- | @M field@ - meter.
class SymFieldMeter repr where
  meter_        :: repr (Meter ctx) -> repr (MidTuneField Ctx_Field)  

data AbcMusic ctx
class SymAbcMusic repr where
  abcmusic :: repr (AbcLine Ctx_Line) -> repr (AbcMusic Ctx_Field)

data AbcLine ctx
class SymAbcLine repr where
  elements          :: repr (a ctx) -> repr (AbcLine Ctx_Line)
  midtuneField      :: repr (MidTuneField Ctx_Field) -> repr (AbcLine Ctx_Line) 
  
  
data Tempo ctx
class SymTempo repr where
  tempo               :: Int -> repr (Tempo ctx)
  ctempo              :: repr (Length ctx) -> Int -> repr (Tempo ctx)
  stempo              :: MeterFraction -> Int -> repr (Tempo ctx)
  
data Length ctx
class SymLength repr where
  ilength             :: Int -> repr (Length ctx)
  flength             :: MeterFraction -> repr (Length ctx) 

  
  
data Key ctx
class SymKey repr where
  key                 :: repr (KeySpec ctx) -> repr (Key ctx)
  highlandNoKey       :: repr (Key ctx)
  highlandMixolydian  :: repr (Key ctx)


data KeySpec ctx
class SymKeySpec repr where
  keySpec :: repr (BaseNote ctx) -> repr (KeySpec ctx)
      
  
data KeyAccidental ctx

class SymKeyAccidental repr where
  keySharp  :: repr (KeyAccidental ctx)
  keyFlat   :: repr (KeyAccidental ctx)
    

class AttrMode ctx
class SymAttrMode repr where
  mode :: (AttrMode a) => String -> repr (a ctx) -> repr (a ctx)


major, minor, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian 
    ::  (AttrMode a, SymAttrMode repr) => repr (a ctx) -> repr (a ctx)   
major         = mode "maj"
minor         = mode "min"
lydian        = mode "lyd"
ionian        = mode "ion"
mixolydian    = mode "mix"
dorian        = mode "dor"
aeolian       = mode "aeo"
phrygian      = mode "phr"
locrian       = mode "loc"



instance AttrMode KeySpec



data Meter ctx
class SymMeter repr where
  meter      :: MeterFraction -> repr (Meter ctx) 
  commonTime :: repr (Meter ctx)
  cutTime    :: repr (Meter ctx)

    

class AttrDuration ctx
class SymAttrDuration repr where
  dur :: (AttrDuration a) => Int -> repr (a ctx)  -> repr (a ctx)     

instance AttrDuration BaseNote
instance AttrDuration Rest



data Rest ctx
class SymRest repr where
  rest :: repr (Rest Ctx_Element)

class AttrOctave ctx
class SymAttrOctave repr where
  octaveLow     :: (AttrOctave a) => Int -> repr (a ctx) -> repr (a ctx) 
  octaveHigh    :: (AttrOctave a) => Int -> repr (a ctx) -> repr (a ctx) 
    
instance AttrOctave BaseNote


class AttrAccidental ctx
class SymAttrAccidental repr where 
  natural       :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  sharp         :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleSharp   :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  flat          :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleFlat    :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)



instance AttrAccidental BaseNote


-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data PitchLetter = C | D | E | F | G | A | B | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Show) 

data BaseNote ctx
class SymBaseNote repr where
  note          :: PitchLetter -> repr (BaseNote Ctx_Element)






data BrokenRhythm ctx
class SymBrokenRhythm repr where
  -- '>' left note dotted, right note halved
  dottedLeft    :: Int -> repr (BrokenRhythm ctx)
    
  -- '<' left note halved, right note dotted 
  dottedRight   :: Int -> repr (BrokenRhythm ctx)   
  

  
data Tie ctx
class SymTie repr where
  tie         :: repr (Tie ctx)

  
      
class AttrGrace ctx
class SymAttrGrace repr where
    tilde       :: (AttrGrace a) => repr (a ctx) -> repr (a ctx)
    stacatto    :: (AttrGrace a) => repr (a ctx) -> repr (a ctx)
    downbow     :: (AttrGrace a) => repr (a ctx) -> repr (a ctx)
    upbow       :: (AttrGrace a) => repr (a ctx) -> repr (a ctx)

instance AttrGrace BaseNote
    
data NPlet ctx
class SymNPlet repr where
  nplet :: Int -> repr (NPlet Ctx_Element)
     
        
data RepeatMark ctx
class SymRepeatMark repr where
  repeatMark :: String -> repr (RepeatMark Ctx_Element)

firstRepeat, secondRepeat, firstEnding, secondEnding 
    :: (SymRepeatMark repr) => repr (RepeatMark Ctx_Element)
firstRepeat   = repeatMark "[1"
secondRepeat  = repeatMark "[2"
firstEnding   = repeatMark "|1"
secondEnding  = repeatMark ":|2"

 

data Slur ctx
class SymSlur repr where
  beginSlur :: repr (Slur Ctx_Element)
  endSlur   :: repr (Slur Ctx_Element)


-- | gracenotes are a prefix attibute of a note
class AttrGraceNotes ctx
class SymAttrGraceNotes repr where
  gracenotes :: (AttrGraceNotes a) => [repr (BaseNote ctx)] -> repr (a ctx) -> repr (a ctx)

instance AttrGraceNotes BaseNote  

data MultiNote ctx
class SymMultiNote repr where
  multinote :: [repr (BaseNote ctx)] -> repr (MultiNote ctx)

  
data TexCommand ctx
class SymTexComamnd repr where
  texCommand :: String -> repr (TexCommand ctx)
  
  