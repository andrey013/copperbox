{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.TextAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output Abc. 
-- Similar to Text.XHTML in the Hierarchical Libraries, but with some extra
-- typefulness due to the Abc phantom type.
--
--------------------------------------------------------------------------------

module HNotate.TextAbc where

import HNotate.CommonUtils

import Data.Monoid
import Data.Ratio
import Text.PrettyPrint.Leijen


-- A phantom type
newtype Abc a = Abc { getAbc :: Doc }

instance WDoc Abc where
  unwrap (Abc a)  = a
  wrap a          = Abc a
  
  
instance Show (Abc a) where
  show (Abc a) = show $ pretty a




type LengthRep a = (a,a)


ppMeter :: Integral a => LengthRep a -> Doc
ppMeter (n,d) =
    group $ integer (fromIntegral n) <> char '/' <> int (fromIntegral d)


ppLengthRep :: (Integral a) => LengthRep a -> Doc
ppLengthRep (n,d) =
    let r       = n % d
        (n',d') = (numerator r, denominator r)
    in f (fromIntegral n') (fromIntegral d')
  where
    f 1 1   = empty       -- the default note length
    f n 1   = int n
    f 1 2   = char '/'    -- short hand for pitch/2
    f n d   = group $ int n <> char '/' <> int d




runAbc (Abc a) = putDoc $ pretty a




--------------------------------------------------------------------------------
-- Tunebook?

data AbcTuneT
type AbcTune = Abc AbcTuneT

tune                :: AbcCxt_Header -> AbcCxt_Body -> AbcTune
tune h b            = caten (<$>) h b

data AbcCxt_HeaderT
type AbcCxt_Header = Abc AbcCxt_HeaderT

header              :: AbcCxt_Header
header              = wrap $ empty

instance Sequence AbcCxt_HeaderT
instance Monoid (Abc AbcCxt_HeaderT) where
  mempty = header
  mappend = caten (<$>)
  
  
instance Append Abc AbcCxt_HeaderT AbcFieldT
instance Append Abc AbcCxt_HeaderT AbcMidTuneFieldT


data AbcCxt_BodyT
type AbcCxt_Body = Abc AbcCxt_BodyT

body                :: AbcCxt_Body
body                = wrap $ empty

instance Sequence AbcCxt_BodyT
instance Monoid (Abc AbcCxt_BodyT) where
  mempty = body
  mappend = caten (<>)
  

instance Append Abc AbcCxt_BodyT AbcMidTuneFieldT


--------------------------------------------------------------------------------
-- * Information fields (3)



data AbcFieldT
type AbcField = Abc AbcFieldT

field :: Char -> Abc a -> AbcField
field ch o = wrap $ group $ char ch <> char ':' <+> unwrap o

-- | @A field@ - area.
area_field                :: String -> AbcField
area_field                = field 'A' . wrap . text


-- | @B field@ - book.
book_field                :: String -> AbcField
book_field                = field 'B' . wrap . text

-- | @C field@ - composer name.
composer_field            :: String -> AbcField
composer_field            = field 'C' . wrap . text

-- | @D field@ - discography.
discography_field         :: String -> AbcField
discography_field         = field 'D' . wrap . text

-- | @G field@ - group.
group_field               :: String -> AbcField
group_field               = field 'G' . wrap . text

-- | @H field@ - history.
history_field             :: [String] -> AbcField
history_field             = field 'H' . wrap . body
  where
    body = align . vcat . map string

-- | @I field@ - information.
information_field         :: String -> AbcField
information_field         = field 'I' . wrap . text

-- | @N field@ - notes.
notes_field               :: String -> AbcField
notes_field               = field 'N' . wrap . text

-- | @O field@ - origin.
origin_field              :: String -> AbcField
origin_field              = field 'O' . wrap . text

-- | @R field@ - rhythm.
rhythm_field              :: String -> AbcField
rhythm_field              = field 'R' . wrap . text

-- | @S field@ - source.
source_field              :: String -> AbcField
source_field              = field 'S' . wrap . text

-- | @X field@ - reference \/ tune number.
number_field              :: Int -> AbcField
number_field              = field 'X' . wrap . int

-- | @Z field@ - transcriber notes.
transcriber_notes_field   :: String -> AbcField
transcriber_notes_field   = field 'Z' . wrap . text


-- Mid tune fields
data AbcMidTuneFieldT
type AbcMidTuneField = Abc AbcMidTuneFieldT

mtfield :: Char -> Abc a -> AbcMidTuneField
mtfield ch o = wrap $ group $ char ch <> char ':' <+> unwrap o



-- | @E field@ - elemskip.
elemskip_field              :: String -> AbcMidTuneField
elemskip_field              = mtfield 'E' . wrap . text

-- | @K field@ - key, note untyped so it can print keys or clef information.

key_field                   :: Abc a -> AbcMidTuneField
key_field                   = mtfield 'K'


-- | @L field@ - unit note length - i.e. the default length.
unit_note_length_field      :: Integral a => LengthRep a -> AbcMidTuneField
unit_note_length_field      = mtfield 'L' . wrap . ppMeter

-- a synonym
l_field                     :: Integral a => LengthRep a -> AbcMidTuneField
l_field                     = unit_note_length_field

-- | @M field@ - meter.
meter_field                 :: Abc a -> AbcMidTuneField
meter_field                 = mtfield 'M'

-- | @P field@ - parts, simplified - parts are just represented as a string.
parts_field                 :: [Char] -> AbcMidTuneField
parts_field                 = mtfield 'P' . wrap . text

-- | @Q field@ - tempo.
tempo_field                 :: Abc a -> AbcMidTuneField
tempo_field                 = mtfield 'Q'

-- | @T field@ - title.
title_field                 :: String -> AbcMidTuneField
title_field                 = mtfield 'T' . wrap . text

-- | @W field@ - words.
words_field                 :: String -> AbcMidTuneField
words_field                 = mtfield 'W' . wrap . text


-- ** M: meter (3.1.6)
data AbcMeterT
type AbcMeter = Abc AbcMeterT

meter               :: (Integer,Integer) -> AbcMeter
meter r             = wrap $ ppMeter r


common_time         :: AbcMeter
common_time         = wrap $  char 'C'

cut_time            :: AbcMeter
cut_time            = wrap $ text "C|"


-- ** Q: tempo (3.1.8)
data AbcTempoT
type AbcTempo = Abc AbcTempoT

tempo               :: Int -> AbcTempo
tempo               = wrap . int

ctempo              :: AbcLength -> Int -> AbcTempo
ctempo l i          = caten (<+>) l (wrap $ int i)

stempo              :: Integral a => LengthRep a -> Int -> AbcTempo
stempo mf i         = wrap $ ppLengthRep mf <+> int i


data AbcLengthT
type AbcLength = Abc AbcLengthT

ilength             :: Integer -> AbcLength
ilength             = wrap . integer

flength             :: Integral a => LengthRep a -> AbcLength
flength             = wrap . ppLengthRep

-- ** K: key (3.1.14)
data AbcKeyT
type AbcKey = Abc AbcKeyT

key                   :: AbcKeySpec -> AbcKey
key                   = promote

highland_no_key       :: AbcKey
highland_no_key       = wrap $ text "HP"

highland_mixolydian   :: AbcKey
highland_mixolydian   = wrap $ text "Hp"

data AbcKeySpecT
type AbcKeySpec = Abc AbcKeySpecT

key_spec :: AbcNote -> AbcMode -> AbcKeySpec
key_spec n m    = caten (<+>) n m

data AbcKeyAccidentalT
type AbcKeyAccidental = Abc AbcKeyAccidentalT

key_sharp   :: AbcKeyAccidental
key_sharp   = wrap $ char '#'


key_flat    :: AbcKeyAccidental
key_flat    = wrap $ char 'b'

data AbcModeT
type AbcMode = Abc AbcModeT

mode :: String -> AbcMode
mode = wrap . text

major, minor, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian
    :: AbcMode
major         = mode "maj"
minor         = mode "min"
lydian        = mode "lyd"
ionian        = mode "ion"
mixolydian    = mode "mix"
dorian        = mode "dor"
aeolian       = mode "aeo"
phrygian      = mode "phr"
locrian       = mode "loc"



--------------------------------------------------------------------------------
-- * The tune elements (4)


-- ** Pitch (4.1)

-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data AbcPitchLetter = C | D | E | F | G | A | B 
                    | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Enum,Ord)

instance Pretty AbcPitchLetter where
  pretty C    = char 'C'
  pretty D    = char 'D'
  pretty E    = char 'E'
  pretty F    = char 'F'
  pretty G    = char 'G'
  pretty A    = char 'A'
  pretty B    = char 'B'
  pretty C2   = char 'c'
  pretty D2   = char 'd'
  pretty E2   = char 'e'
  pretty F2   = char 'f'
  pretty G2   = char 'g'
  pretty A2   = char 'a'
  pretty B2   = char 'b'

instance Show AbcPitchLetter where
  show = show . pretty
  
  
data AbcNoteT
type AbcNote = Abc AbcNoteT

note          :: AbcPitchLetter -> AbcNote
note          = wrap . pretty

instance Append Abc AbcCxt_BodyT AbcNoteT

data AbcOctaveT
type AbcOctave = Abc AbcOctaveT

octaveHigh    :: Int -> AbcOctave
octaveHigh i  = wrap $ text (replicate i '\'')


octaveLow     :: Int -> AbcOctave
octaveLow i   = wrap $ text (replicate i ',')


instance SuffixAttr AbcNoteT AbcOctaveT

-- ** Accidentals (4.2)
data AbcAccidentalT
type AbcAccidental = Abc AbcAccidentalT

natural       :: AbcAccidental
natural       = wrap $ char '='

sharp         :: AbcAccidental
sharp         = wrap $ char '^'

doubleSharp   :: AbcAccidental
doubleSharp   = wrap $ string "^^"

flat          :: AbcAccidental
flat          = wrap $ char '_'

doubleFlat    :: AbcAccidental
doubleFlat    = wrap $ string "__"

instance PrefixAttr AbcNoteT AbcAccidentalT


-- ** Note lengths (4.3)
data AbcDurationT
type AbcDuration = Abc AbcDurationT

dmult             :: Int -> AbcDuration
dmult             = wrap . int

ddiv1             :: Int -> AbcDuration
ddiv1 i           = wrap (char '/' <> int i)

ddiv2             :: (Int,Int) -> AbcDuration
ddiv2 (n,d)       = wrap (int n <> char '/' <> int d)


instance SuffixAttr AbcNoteT AbcDurationT
instance SuffixAttr AbcRestT AbcDurationT

-- ** Broken rhythm (4.4)
data AbcBrokenRhythmT
type AbcBrokenRhythm = Abc AbcBrokenRhythmT

-- '>' left note dotted, right note halved
dotted_left       :: AbcBrokenRhythm
dotted_left       = wrap $ char '>'

dotted_leftn      :: Int -> AbcBrokenRhythm
dotted_leftn i    = wrap $ text $ replicate i '>'


-- '<' left note halved, right note dotted
dotted_right      :: AbcBrokenRhythm
dotted_right      = wrap $ char '<'


dotted_rightn     :: Int -> AbcBrokenRhythm
dotted_rightn i   = wrap $ text $ replicate i '<'


-- ** Rests (4.5)
data AbcRestT
type AbcRest = Abc AbcRestT

rest                :: AbcRest
rest                = wrap $ char 'z'

spacer              :: AbcRest
spacer              = wrap $ char 'x'

instance Append Abc AbcCxt_BodyT AbcRestT




-- ** Repeat \/ bar symbols & First and second repeats (4.8 & 4.9)
data AbcRepeatMarkT
type AbcRepeatMark = Abc AbcRepeatMarkT

repeatMark :: String -> AbcRepeatMark
repeatMark = wrap . text


instance Append Abc AbcCxt_BodyT AbcRepeatMarkT


-- ** Ties and slurs (4.11)
data AbcTieT
type AbcTie = Abc AbcTieT

tie             :: AbcTie
tie             = wrap $ char '-'

instance Append Abc AbcCxt_BodyT AbcTieT

data AbcSlurT
type AbcSlur = Abc AbcSlurT

beginSlur       :: AbcSlur
beginSlur       = wrap $ lparen

endSlur         :: AbcSlur
endSlur         = wrap $ rparen

instance Append Abc AbcCxt_BodyT AbcSlurT


-- ** Grace notes (4.12)
data AbcGraceNotesT
type AbcGraceNotes = Abc AbcGraceNotesT



gracenotes          :: [AbcNote] -> AbcGraceNotes
gracenotes          = wrap . encloseSep lbrace rbrace empty . map unwrap


-- Its simpler if we make gracenotes a glyph rather than
-- a prefix attr of a note.
instance Append Abc AbcCxt_BodyT AbcGraceNotesT

-- ** Duplets, triplets, quadruplets, etc. (4.13)
data AbcNPletT
type AbcNPlet = Abc AbcNPletT

nplet               :: Int -> AbcNPlet
nplet               = wrap . group . (char '(' <>) . int

instance Append Abc AbcCxt_BodyT AbcNPletT



-- ** Decorations (4.14)
data AbcDecorationT
type AbcDecoration = Abc AbcDecorationT

tilde               :: AbcDecoration
tilde               = wrap $ char '~'

stacatto            :: AbcDecoration
stacatto            = wrap $ char '.'

downbow             :: AbcDecoration
downbow             = wrap $ char 'v'

upbow               :: AbcDecoration
upbow               = wrap $ char 'u'

instance PrefixAttr AbcNoteT AbcDecorationT


-- ** Chords and unisons (4.17)
data AbcChordT
type AbcChord = Abc AbcChordT

chord           :: [AbcNote] -> AbcChord
chord           = wrap . encloseSep lbracket rbracket empty . map unwrap

instance Append Abc AbcCxt_BodyT AbcChordT

-- * Clefs (6)
data AbcClefT
type AbcClef = Abc AbcClefT

clef                :: AbcClefName -> AbcClef
clef cn             = wrap $ text "clef=" <> unwrap cn

data AbcClefNameT
type AbcClefName = Abc AbcClefNameT

clef_name           :: String -> AbcClefName
clef_name           = wrap . text

treble              :: AbcClefName
treble              = clef_name "treble"

alto                :: AbcClefName
alto                = clef_name "alto"

tenor               :: AbcClefName
tenor               = clef_name "tenor"

bass                :: AbcClefName
bass                = clef_name "bass"

perc                :: AbcClefName
perc                = clef_name "perc"


-- * Multiple voices (7)
-- ** Voice overlay (7.4)

-- type must be - AbcCxt_Element -> AbcCxt_Element  -> AbcCxt_Element
-- for folding

(&\) :: AbcCxt_Body -> AbcCxt_Body -> AbcCxt_Body
(&\) a b = caten3 (<>) a (wrap $ text " &\\" <> line) b



-- * Named elements

-- Notes
c_, d_, e_, f_, g_, a_, b_ :: AbcNote
c_  = note C
d_  = note D
e_  = note E
f_  = note F
g_  = note G
a_  = note A
b_  = note B

c__, d__, e__, f__, g__, a__, b__ :: AbcNote
c__  = note C2
d__  = note D2
e__  = note E2
f__  = note F2
g__  = note G2
a__  = note A2
b__  = note B2

-- Rests

-- @z1@ - a rest of the default note length.
z1                  :: AbcRest
z1                  = rest ! dmult 1

-- @z1@ - a rest of double the default note length.
z2                  :: AbcRest
z2                  = rest ! dmult 2

-- @z4@ - a rest four times the default note length.
z4                  :: AbcRest
z4                  = rest ! dmult 4

-- @z'2@ - a rest of half the default note length.
z'2                 :: AbcRest
z'2                 = rest ! ddiv1 2

-- repeats and barlines

-- | @barline@ - single stroke @|@.
barline             :: AbcRepeatMark
barline             = repeatMark "|"

-- | @thinThick@ - @|]@.
thinThick           :: AbcRepeatMark
thinThick           = repeatMark "|]"

-- | @thickThin@ - @|]@.
thickThin           :: AbcRepeatMark
thickThin           = repeatMark "[|"

-- | @beginRepeat@ - @|:@.
beginRepeat         :: AbcRepeatMark
beginRepeat         = repeatMark "|:"

-- | @endRepeat@ - @|:@.
endRepeat           :: AbcRepeatMark
endRepeat           = repeatMark ":|"

-- | @doubleRepeat@ - @::@.
doubleRepeat        :: AbcRepeatMark
doubleRepeat        = repeatMark "::"


-- | @firstRepeat@ - @[1@.
firstRepeat         :: AbcRepeatMark
firstRepeat         = repeatMark "[1"

-- | @secondRepeat@ - @[2@.
secondRepeat        :: AbcRepeatMark
secondRepeat        = repeatMark "[2"

-- | @firstEnding@ - @|1@.
firstEnding         :: AbcRepeatMark
firstEnding         = repeatMark "|1"


-- | @secondEnding@ - @:|2@.
secondEnding        :: AbcRepeatMark
secondEnding        = repeatMark ":|2"



