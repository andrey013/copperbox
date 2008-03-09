
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Abc.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for ABC format
-- |
--------------------------------------------------------------------------------

module Bala.Format.Abc.Datatypes  where


type AbcFile = [AbcFileElement]

data AbcFileElement
    = AbcTuneElement       AbcTune
    | AbcTexCommandElement TexCommand
    | AbcFileFieldsElement [Field]
  deriving (Eq, Show)


--------------------------------------------------------------------------------

type AbcTune = (AbcHeader, AbcMusic)


data AbcHeader = AbcHeader {
    field_number  :: Field,       -- 'X' field
    field_title   :: [Field],     -- 'T' fields
    other_fields  :: [Field],
    field_key     :: Field        -- 'K' field
  }
  deriving (Eq, Show)

data Field 
    = FieldFile           String
    | FieldNumber         Int
    | FieldTitle          String
    | FieldArea           String
    | FieldBook           String
    | FieldComposer       String
    | FieldDiscography    String
    | FieldElemskip       String
    | FieldGroup          String
    | FieldHistory        [String]
    | FieldInformation    String
    | FieldDefaultLength  NoteLengthStrict
    | FieldMeter          Meter
    | FieldNotes          String
    | FieldOrigin         String
    | FieldParts          [Part]
    | FieldTempo          Tempo
    | FieldRhythm         String
    | FieldSource         String
    | FieldTranscrNotes   String
    | FieldKey            Key
    | FieldPart           Part
    | FieldWords          String
  deriving (Eq, Show)

data Key
    = Key KeySpec 
    | HighlandNoKey          -- HP
    | HighlandMixolydian     -- Hp
  deriving (Eq, Show)

type KeySpec = (KeyNote, Maybe ModeSpec, [GlobalAccidental])

type KeyNote = (BaseNote, Maybe KeyAccidental)  

data KeyAccidental
    = KeySharp
    | KeyFlat
  deriving (Eq, Show)

type ModeSpec = (Mode, String)

data Mode 
    = ModeMinor
    | ModeMajor
    | ModeLydian
    | ModeIonian
    | ModeMixolydian
    | ModeDorian
    | ModeAeolian
    | ModePhrygian
    | ModeLocrian
  deriving (Eq, Show)      

data Meter
    = Meter MeterFraction
    | MeterCommonTime       -- 'C'
    | MeterCutTime          -- "C|"
  deriving (Eq, Show)

type MeterFraction = (Int,Int)
  
data Tempo
    = Tempo Int
    | TempoC NoteLength Int
    | TempoAbsolute NoteLengthStrict Int
  deriving (Eq, Show)  
  
type NoteLengthStrict = (Int, Int)

-- parts can be nested like a rose tree
data Part
    = PartTree Int [Part]
    | PartElem Int Char
  deriving (Eq, Show)
  
--------------------------------------------------------------------------------

type AbcMusic = [AbcLine]

data AbcLine
    = Elements        [Element]
    | MidTexCommand   TexCommand
    | MidTuneField    Field
  deriving (Eq, Show)

data Element 
    = NoteElement   NoteElement
    | TupletElement TupletSpec [NoteElement]
    | Barline       Barline
    | NthRepeat     RepeatMark
    | Slur          Slur
    | Space
    | UserDefined   String
  deriving (Eq, Show)
    
type TupletSpec = [Int]

type NoteElement = (NoteStem, Maybe BrokenRhythm)

type NoteStem 
  = (Maybe GuitarChord, Maybe GraceNotes, [Gracing], [Note])
  
data Note = Note {
    note_value      :: NoteOrRest,
    opt_note_length :: Maybe NoteLength, 
    opt_tie         :: Maybe Tie
  }
  deriving (Eq,Show)



data NoteOrRest
    = NotePitch PitchSpec
    | Rest
  deriving (Eq, Show)
  
data PitchSpec = PitchSpec {
    pitch_base      :: BaseNote,
    opt_accidental  :: Maybe Accidental,
    opt_octave_mark :: Maybe Octave 
  }
  deriving (Eq,Show)        

data Octave
    = OctaveLow  Int
    | OctaveHigh Int
  deriving (Eq, Show)
  
data Accidental
    = Natural
    | Sharp
    | DoubleSharp
    | Flat
    | DoubleFlat
  deriving (Eq, Show)


type BaseNote = Char

data BrokenRhythm
    = DottedLeft  Int    -- '>' left note dotted, right note halved
    | DottedRight Int    -- '<' left note halved, right note dotted
  deriving (Eq, Show)  

  
data Tie = Tie
  deriving (Eq, Show)

data Gracing
    = Tilde
    | Stacatto
    | DownBow
    | UpDown
  deriving (Eq, Show)
  
type GraceNotes = [PitchSpec]

-----

data GuitarChord 
    = FormalChord BaseNote (Maybe ChordType) (Maybe BaseNote)
    | UninterpretedChord String
  deriving (Eq, Show)
  


data Barline 
    = BarSingle
    | BarDouble
    | BarThickThin
    | BarThinThick
    | RepeatLeft
    | RepeatRight
    | RepeatBoth
  deriving (Eq, Show)

data RepeatMark 
    = RepeatFirst
    | RepeatSecond
    | EndingFirst
    | EndingSecond
  deriving (Eq, Show)
  
data Slur
    = SlurBegin
    | SlurEnd
  deriving (Eq, Show)    
    


type GlobalAccidental = (Accidental, BaseNote)

type NoteLength = (Int, Maybe Int)

type ChordType = String

type TexCommand = String


