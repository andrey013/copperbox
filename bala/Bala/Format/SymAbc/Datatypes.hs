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



data Field ctx
data MidTuneField ctx 

--------------------------------------------------------------------------------
-- * Fields

-- | @X field@ - reference \/ tune number.
class SymFieldNumber repr where
  num_                :: Int -> repr (Field CT_Field)

-- | @T field@ - title. 
class SymFieldTitle repr where
  title_              :: String -> repr (MidTuneField CT_Field)

-- | @A field@ - area.
class SymFieldArea repr where
  area_               :: String -> repr (Field CT_Field)

-- | @B field@ - book.
class SymFieldBook repr where  
  book_               :: String -> repr (Field CT_Field)

-- | @C field@ - composer name.
class SymFieldComposer repr where  
  composer_           :: String -> repr (Field CT_Field)

-- | @D field@ - discography.
class SymFieldDiscography repr where  
  discography_        :: String -> repr (Field CT_Field)

-- | @E field@ - elemskip.
class SymFieldElemskip repr where
  elemskip_           :: String -> repr (MidTuneField CT_Field)

-- | @G field@ - group.
class SymFieldGroup repr where  
  group_              :: String -> repr (Field CT_Field)

-- | @I field@ - information.
class SymFieldInformation repr where
  information_        :: String -> repr (Field CT_Field)

-- | @N field@ - notes.
class SymFieldNotes repr where  
  notes_              :: String -> repr (Field CT_Field)

-- | @O field@ - origin.
class SymFieldOrigin repr where  
  origin_             :: String -> repr (Field CT_Field)

-- | @R field@ - rhythm.
class SymFieldRhythm repr where  
  rhythm_             :: String -> repr (Field CT_Field)

-- | @S field@ - source.
class SymFieldSource repr where  
  source_             :: String -> repr (Field CT_Field)

-- | @W field@ - words.
class SymFieldWords repr where    
  words_              :: String -> repr (MidTuneField CT_Field)

-- | @Z field@ - transcriber notes.  
class SymFieldTranscrNotes repr where
  transcrNotes_   :: String -> repr (Field CT_Field)

-- | @H field@ - history.
class SymFieldHistory repr where
  history_      :: [String] -> repr (Field CT_Field)

-- | @K field@ - key.
class SymFieldKey repr where
  key_          :: repr (Key ctx) -> repr (MidTuneField CT_Field)
  
-- | @L field@ - default note length.
class SymFieldDefaultNoteLength repr where
  defaultNoteLength_    :: MeterFraction -> repr (MidTuneField CT_Field)


-- | @P field@ - parts, simplified - parts are just represented as a string.
class SymFieldParts repr where 
  parts_        :: [Char] -> repr (MidTuneField CT_Field)
  
-- | @Q field@ - tempo.
class SymFieldTempo repr where
  tempo_        :: repr (Tempo ctx) -> repr (MidTuneField CT_Field)

-- | @M field@ - meter.
class SymFieldMeter repr where
  meter_        :: repr (Meter ctx) -> repr (MidTuneField CT_Field)  

data AbcMusic ctx
class SymAbcMusic repr where
  abcmusic :: repr (AbcLine CT_Line) -> repr (AbcMusic CT_Field)

data AbcLine ctx
class SymAbcLine repr where
  elements          :: repr (a ctx) -> repr (AbcLine CT_Line)
  midtuneField      :: repr (MidTuneField CT_Field) -> repr (AbcLine CT_Line) 
  
  
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
    

data Mode ctx
class SymMode repr where
  mode :: String -> repr (Mode ctx)


instance Attribute KeySpec Mode 



data Meter ctx
class SymMeter repr where
  meter      :: MeterFraction -> repr (Meter ctx) 
  commonTime :: repr (Meter ctx)
  cutTime    :: repr (Meter ctx)

    

data Duration ctx
class SymDuration repr where
  dur :: Int -> repr (Duration ctx)     

instance Attribute BaseNote Duration
instance Attribute Rest Duration



data Rest ctx
class SymRest repr where
  rest :: repr (Rest CT_Element)

data Octave ctx
class SymOctave repr where
  octaveLow     :: Int -> repr (Octave ctx) 
  octaveHigh    :: Int -> repr (Octave ctx)
    
instance Attribute BaseNote Octave


data Accidental ctx
class SymAccidental repr where 
  natural       :: repr (Accidental ctx)
  sharp         :: repr (Accidental ctx)
  doubleSharp   :: repr (Accidental ctx)
  flat          :: repr (Accidental ctx)
  doubleFlat    :: repr (Accidental ctx)



instance Attribute BaseNote Accidental


-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data PitchLetter = C | D | E | F | G | A | B | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Show) 

data BaseNote ctx
class SymBaseNote repr where
  note          :: PitchLetter -> repr (BaseNote CT_Element)






data BrokenRhythm ctx
class SymBrokenRhythm repr where
  -- '>' left note dotted, right note halved
  dottedLeft    :: Int -> repr (BrokenRhythm ctx)
    
  -- '<' left note halved, right note dotted 
  dottedRight   :: Int -> repr (BrokenRhythm ctx)   
  

  
data Tie ctx
class SymTie repr where
  tie         :: repr (Tie ctx)

  
      
data Grace ctx
class SymGrace repr where
    tilde       :: repr (Grace ctx)
    stacatto    :: repr (Grace ctx)
    downbow     :: repr (Grace ctx)
    upbow       :: repr (Grace ctx)

instance Attribute BaseNote Grace
    
data NPlet ctx
class SymNPlet repr where
  nplet :: Int -> repr (NPlet CT_Element)
     
        
data RepeatMark ctx
class SymRepeatMark repr where
  repeatMark :: String -> repr (RepeatMark CT_Element)



 

data Slur ctx
class SymSlur repr where
  beginSlur :: repr (Slur CT_Element)
  endSlur   :: repr (Slur CT_Element)


-- | gracenotes are a prefix attibute of a note
data GraceNotes ctx
class SymGraceNotes repr where
  gracenotes :: [repr (BaseNote ctx)] -> repr (GraceNotes ctx)

instance Attribute BaseNote GraceNotes  

data MultiNote ctx
class SymMultiNote repr where
  multinote :: [repr (BaseNote ctx)] -> repr (MultiNote ctx)

  
data TexCommand ctx
class SymTexComamnd repr where
  texCommand :: String -> repr (TexCommand ctx)
  
  