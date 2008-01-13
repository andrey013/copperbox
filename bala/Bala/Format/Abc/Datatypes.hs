
module Sound.Bala.Format.Abc.Datatypes  where


type AbcFile = [AbcFileElement]

data AbcFileElement
    = AbcTuneElement AbcTune
    | AbcTexCommandElement Tex_command
    | AbcFileFieldsElement [AbcField]
  deriving (Eq, Show)


--------------------------------------------------------------------------------

type AbcTune = (AbcHeader, AbcMusic)


data AbcHeader = AbcHeader {
        field_number :: AbcField,       -- 'X' field
        field_title :: [AbcField],      -- 'T' fields
        other_fields :: [AbcField],
        field_key :: AbcField           -- 'K' field
        }
  deriving (Eq, Show)

data AbcField 
    = AbcFileField String
    | AbcNumberField Int
    | AbcTitleField String
    | AbcAreaField String
    | AbcBookField String
    | AbcComposerField String
    | AbcDiscographyField String
    | AbcElemskipField String
    | AbcGroupField String
    | AbcHistoryField [String]
    | AbcInformationField String
    | AbcDefaultLengthField Note_length_strict
    | AbcMeterField AbcMeter
    | AbcNotesField String
    | AbcOriginField String
    | AbcPartsField [AbcPart]
    | AbcTempoField AbcTempo
    | AbcRhythmField String
    | AbcSourceField String
    | AbcTranscrNotesField String
    | AbcKeyField AbcKey
    | AbcPartField AbcPart
    | AbcWordsField String
  deriving (Eq, Show)

data AbcKey
    = AbcKey Key_spec 
    | AbcHighlandNoKey          -- HP
    | AbcHighlandMixolydian     -- Hp
  deriving (Eq, Show)

type Key_spec = (Key_note, Maybe Mode_spec, [Global_accidental])

type Key_note = (Basenote, Maybe AbcKeyAccidental)  

data AbcKeyAccidental
    = AbcSharpKey
    | AbcFlatKey
  deriving (Eq, Show)

type Mode_spec = (AbcMode, String)

data AbcMode 
    = AbcMinorMode
    | AbcMajorMode
    | AbcLydianMode
    | AbcIonianMode
    | AbcMixolydianMode
    | AbcDorianMode
    | AbcAeolianMode
    | AbcPhrygianMode
    | AbcLocrianMode
  deriving (Eq, Show)      

data AbcMeter
    = AbcMeter Int Int
    | AbcCommonTimeMeter    -- 'C'
    | AbcCutTimeMeter       -- "C|"
  deriving (Eq, Show)
  
data AbcTempo
    = AbcTempo Int
    | AbcCTempo Note_length  Int
    | AbcAbsoluteTempo Note_length_strict Int
  deriving (Eq, Show)  
  
type Note_length_strict = (Int, Int)

-- parts can be nested like a rose tree
data AbcPart
    = AbcPartTree Int [AbcPart]
    | AbcPartElem Int Char
  deriving (Eq, Show)
  
--------------------------------------------------------------------------------

type AbcMusic = [AbcLine]

data AbcLine
    = AbcElements [AbcElement]
    | AbcMidTexCommand Tex_command
    | AbcMidTuneField AbcField
  deriving (Eq, Show)

data AbcElement 
    = AbcNoteElement Note_element
    | AbcTupletElement Tuplet_spec [Note_element]
    | AbcBarlineElement AbcBarline
    | AbcRepeatElement AbcRepeatMark
    | AbcSlurElement AbcSlur
    | AbcSpaceElement
    | AbcUserDefinedElement String
  deriving (Eq, Show)
    
type Tuplet_spec = [Int]

type Note_element = (Note_stem, Maybe AbcBrokenRhythm)

type Note_stem 
  = (Maybe AbcGuitarChord, Maybe Grace_notes, [AbcGracing], [AbcNote])
  
data AbcNote = AbcNote {
      note_value :: AbcValue,
      opt_note_length :: Maybe Note_length, 
      opt_tie :: Maybe AbcTie
      }
  deriving (Eq,Show)


-- aka Note or rest
data AbcValue
    = AbcPitchValue AbcPitch
    | AbcRest
  deriving (Eq, Show)
  
data AbcPitch = AbcPitch {
        pitch_base :: Basenote,
        opt_accidental :: Maybe AbcAccidental,
        opt_octave_mark :: Maybe AbcOctave 
        }
  deriving (Eq,Show)        

data AbcOctave
    = AbcLowOctave Int
    | AbcHighOctave Int
  deriving (Eq, Show)
  
data AbcAccidental
    = AbcSharp
    | AbcDoubleSharp
    | AbcFlat
    | AbcDoubleFlat
    | AbcNatural
  deriving (Eq, Show)


type Basenote = Char

data AbcBrokenRhythm
    = AbcDottedLeft Int     -- '>' left note dotted, right note halved
    | AbcDottedRight Int    -- '<' left note halved, right note dotted
  deriving (Eq, Show)  

  
data AbcTie = AbcTie
  deriving (Eq, Show)

data AbcGracing
    = AbcTilde
    | AbcStacatto
    | AbcDownBow
    | AbcUpDown
  deriving (Eq, Show)
  
type Grace_notes = [AbcPitch]

-----

data AbcGuitarChord 
    = AbcFormalChord Basenote (Maybe Chord_type) (Maybe Basenote)
    | AbcUninterpretedChord String
  deriving (Eq, Show)
  


data AbcBarline 
    = AbcSingleBar
    | AbcDoubleBar
    | AbcThick_ThinBar
    | AbcThin_ThickBar
    | AbcLeftRepeat
    | AbcRightRepeat
    | AbcBothRepeat
  deriving (Eq, Show)

data AbcRepeatMark 
    = AbcFirstRepeat
    | AbcSecondRepeat
    | AbcFirstEnding
    | AbcSecondEnding
  deriving (Eq, Show)
  
data AbcSlur
    = AbcBeginSlur
    | AbcEndSlur
  deriving (Eq, Show)    
    


type Global_accidental = (AbcAccidental, Basenote)

type Note_length = (Int, Maybe Int)

type Chord_type = String

type Tex_command = String


