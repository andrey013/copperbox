
module Sound.Abc.Datatypes  where


type AbcFile = [FileElement]

data FileElement  = Tune Tune
                  | Command Tex_command
                  | Field Field
                  deriving (Eq, Show)

type Tune = (Header, [Abc_line])


type Header = (Field, [Field], [Field], Field)

data Accidental = SHARP | DBL_SHARP | FLAT | DBL_FLAT | NATURAL
  deriving (Eq, Show)

data Mode = MIN | MAJ | LYD | ION | MIX | DOR | AEO | PHR | LOC
  deriving (Eq, Show)
  
data Key_accidental = KEY_SHARP | KEY_FLAT
  deriving (Eq, Show)
  
data Key = Key Key_spec | HP_NO_KEY | Hp
  deriving (Eq, Show)
  
data Tie = TIE
  deriving (Eq, Show)
  
data Octave = Octave_low Int | Octave_hi Int
  deriving (Eq, Show)
  
data Note_or_rest = Pitch Pitch | Rest
  deriving (Eq, Show)

-- parts can be nested like a rose tree
data Part = PartTree Int [Part] | PartElem Int Char
  deriving (Eq, Show)
  
data Slur = SLUR_BEGIN | SLUR_END
  deriving (Eq, Show)
  
data Broken_rhythm = DOT_NEXT Int | DOT_PREV Int
  deriving (Eq, Show)
  
data Barline 
	= BARLINE
	| BAR_DBL
	| BAR_THICK_THIN
	| BAR_THIN_THICK
	| REP_LEFT
	| REP_RIGHT
	| REP_BOTH
  deriving (Eq, Show)

data Guitar_chord = Formal_chord Basenote (Maybe Chord_type) (Maybe Basenote)
                  | Chord_text String
  deriving (Eq, Show)
                    
data Gracing = TILDE | STACATTO | DOWN_BOW | UP_BOW
  deriving (Eq, Show)
  
data Nth_repeat 
	= FIRST_REPEAT
    | SECOND_REPEAT
    | FIRST_ENDING
    | SECOND_ENDING
  deriving (Eq, Show)
      
data Element 
	= Note_element Note_element
    | Tuplet_element Tuplet_spec [Note_element]
    | Barline Barline
    | Nth_repeat Nth_repeat
    | Slur Slur
    | Space
    | UserDefined String
  deriving (Eq, Show)
  
             
data Field 
	= Field_file String
	| Field_number Int
	| Field_title String
	| Field_area String
	| Field_book String
	| Field_composer String
	| Field_discography String
	| Field_elemskip String
	| Field_group String
	| Field_history [String]
	| Field_information String
	| Field_default_length Note_length
	| Field_meter Meter
	| Field_notes String
	| Field_origin String
	| Field_parts [Part]
	| Field_tempo Tempo
	| Field_rhythm String
	| Field_source String
	| Field_transcrnotes String
	| Field_key Key
	| Field_part Part
	| Field_words String
  deriving (Eq, Show)
  
data Abc_line 
	= Element_list [Element]
    | Tex_command Tex_command
    | Mid_tune_field Field
  deriving (Eq, Show)    
              
data Meter = Meter Int Int | COMMON_TIME | CUT_TIME
  deriving (Eq, Show)
  
data Tempo 
	= Tempo Int
    | C_tempo Note_length  Int
    | Absolute_tempo Note_length_strict Int
  deriving (Eq, Show)           
  

type Basenote = Char

type Global_accidental = (Accidental, Basenote)

type Mode_spec = (Mode, String)

type Key_note = (Basenote, Maybe Key_accidental)

type Key_spec = (Key_note, Maybe Mode_spec, [Global_accidental])

type Pitch = (Maybe Accidental, Basenote, Maybe Octave)

type Note_length_strict = (Int, Int)

type Note_length = (Int, Maybe Int)

type Note = (Note_or_rest, Maybe Note_length, Maybe Tie)

type Grace_notes = [Pitch]

type Tuplet_spec = [Int]

type Chord_type = String

type Note_stem 
	= (Maybe Guitar_chord, Maybe Grace_notes, [Gracing], [Note])
                  
type Note_element = (Note_stem, Maybe Broken_rhythm)

type Tex_command = String

type Abc_music = [Abc_line]



