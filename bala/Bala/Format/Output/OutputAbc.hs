{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Output.OutputAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Output combinators for Abc. 
-- Similar to Text.XHTML in the Hierarchical Libraries, but with some extra
-- typefulness due to the Abc phantom type.
--
--------------------------------------------------------------------------------

module Bala.Format.Output.OutputAbc where

import Bala.Format.Output.OutputBase
import Bala.Base.Meter


import Data.Ratio
import Data.Sequence ( (|>) )
import Text.PrettyPrint.Leijen



-- A phantom type
newtype Abc a = Abc { unAbc :: Skeleton Doc }


-- A type constrained add-right (|>)
class Append cxts cxta

infixl 5 +++

(+++) :: (Append cxts cxta) => Abc cxts -> Abc cxta -> Abc cxts  
(+++) (Abc (Sequence op sq)) (Abc a) = Abc $ Sequence op (sq |> a)
(+++)  _                      _      = error "can't append to a non sequence"


class SuffixAttr cxte cxta

infixl 7 !

( ! ) :: (SuffixAttr cxte cxta) => Abc cxte -> Abc cxta -> Abc cxte
( ! ) (Abc e) (Abc a) = Abc $ Attr (<>) e a

class PrefixAttr cxte cxta

infixl 7 !>
( !> ) :: (PrefixAttr cxte cxta) => Abc cxta -> Abc cxte ->  Abc cxte
( !> ) (Abc a) (Abc e) = Abc $ Attr (flip (<>)) e a





instance Pretty MeterFraction where
  pretty =  ppRatio . uncurry (%) . unMeterFraction


ppMeter :: MeterFraction -> Doc
ppMeter mf = let (n,d) = unMeterFraction mf in
    group $ int (fromIntegral n) <> char '/' <> int (fromIntegral d)
    

ppRatio :: (Integral a) => Ratio a -> Doc
ppRatio r = let (n,d) = (numerator r, denominator r) in
    group $ int (fromIntegral n) <> char '/' <> int (fromIntegral d)

ppNoteLength :: MeterFraction -> Doc
ppNoteLength mf = 
    let r     = uncurry (%) $ unMeterFraction mf
        (n,d) = (numerator r, denominator r) 
    in f n d
  where 
    f 1 1   = empty       -- the default note length
    f n 1   = int n
    f 1 2   = char '/'    -- short hand for pitch/2
    f n d   = group $ int n <> char '/' <> int d
    
    


runAbc (Abc a) = run a

abcLiteral = Abc . literal

abcSeq2 :: Caten Doc -> Abc a -> Abc b -> Abc c
abcSeq2 op (Abc a) (Abc b) = Abc $ sequenceL op [a,b]



--------------------------------------------------------------------------------
-- * Information fields (3)



data CT_Header
header :: Abc CT_Header
header = Abc $ sequenceS (<$>) emptyseq

instance Append CT_Header CT_Field
instance Append CT_Header CT_MidTuneField


data CT_Field

field :: Char -> Abc a -> Abc CT_Field
field ch o = Abc $ sequenceL (<+>) [field_id, unAbc o]
  where field_id = literalP [ch,':']
  
-- | @A field@ - area.
area_field                :: String -> Abc CT_Field
area_field                = field 'A' . Abc . literal . text


-- | @B field@ - book.
book_field                :: String -> Abc CT_Field
book_field                = field 'B' . Abc . literal . text

-- | @C field@ - composer name. 
composer_field            :: String -> Abc CT_Field
composer_field            = field 'C' . Abc . literal . text

-- | @D field@ - discography.
discography_field         :: String -> Abc CT_Field
discography_field         = field 'D' . Abc . literal . text

-- | @G field@ - group.
group_field               :: String -> Abc CT_Field
group_field               = field 'G' . Abc . literal . text

-- | @H field@ - history.
history_field             :: [String] -> Abc CT_Field
history_field             = field 'H' . Abc . body
  where
    body =  nested' align . sequenceL (<$>) . map (literal . text)
  
-- | @I field@ - information.
information_field         :: String -> Abc CT_Field
information_field         = field 'I' . Abc . literal . text

-- | @N field@ - notes.  
notes_field               :: String -> Abc CT_Field
notes_field               = field 'N' . Abc . literal . text

-- | @O field@ - origin. 
origin_field              :: String -> Abc CT_Field
origin_field              = field 'O' . Abc . literal . text

-- | @R field@ - rhythm. 
rhythm_field              :: String -> Abc CT_Field
rhythm_field              = field 'R' . Abc . literal . text

-- | @S field@ - source.
source_field              :: String -> Abc CT_Field
source_field              = field 'S' . Abc . literal . text
           
-- | @X field@ - reference \/ tune number.
number_field              :: Int -> Abc CT_Field
number_field              = field 'X' . Abc . literal . int
  
-- | @Z field@ - transcriber notes.  
transcriber_notes_field   :: String -> Abc CT_Field
transcriber_notes_field   = field 'Z' . Abc . literal . text  


-- Mid tune fields
data CT_MidTuneField

mtfield :: Char -> Abc a -> Abc CT_MidTuneField
mtfield ch o = Abc $ sequenceL (<+>) [field_id, unAbc o]
  where field_id = literalP [ch,':']
  
  

-- | @E field@ - elemskip.
elemskip_field              :: String -> Abc CT_MidTuneField
elemskip_field              = mtfield 'E' . Abc . literal . text

-- | @K field@ - key.
key_field                   :: Abc a -> Abc CT_MidTuneField
key_field                   = mtfield 'K' 
  
-- | @L field@ - default note length.
default_note_length_field   :: MeterFraction -> Abc CT_MidTuneField
default_note_length_field   = mtfield 'L' . Abc . literal . pretty

-- a synonym
l_field                     :: MeterFraction -> Abc CT_MidTuneField
l_field                     = default_note_length_field

-- | @M field@ - meter.
meter_field                 :: Abc a -> Abc CT_MidTuneField
meter_field                 = mtfield 'M' 
  
-- | @P field@ - parts, simplified - parts are just represented as a string.
parts_field                 :: [Char] -> Abc CT_MidTuneField
parts_field                 = mtfield 'P' . Abc . literal . text
  
-- | @Q field@ - tempo.
tempo_field                 :: Abc a -> Abc CT_MidTuneField
tempo_field                 = mtfield 'Q' 

-- | @T field@ - title.
title_field                 :: String -> Abc CT_MidTuneField
title_field                 = mtfield 'T' . Abc . literal . text
 
-- | @W field@ - words.  
words_field                 :: String -> Abc CT_MidTuneField
words_field                 = mtfield 'W' . Abc . literal . text


-- ** M: meter (3.1.6)
data Meter

meter               :: MeterFraction -> Abc Meter
meter r             = abcLiteral $ ppMeter r
  
  
common_time         :: Abc Meter
common_time         = abcLiteral $  char 'C'

cut_time            :: Abc Meter
cut_time            = abcLiteral $ text "C|"  
  

-- ** Q: tempo (3.1.8)
data Tempo

tempo               :: Int -> Abc Tempo
tempo               = abcLiteral . int

ctempo              :: Abc Length -> Int -> Abc Tempo
ctempo l i          = abcSeq2 (<+>) l (abcLiteral $ int i)
 
stempo              :: MeterFraction -> Int -> Abc Tempo
stempo mf i         = abcSeq2 (<+>) (abcLiteral $ pretty mf) (abcLiteral $ int i)


data Length

ilength             :: Int -> Abc Length
ilength             = abcLiteral . int

flength             :: MeterFraction -> Abc Length
flength             = abcLiteral . pretty
  
-- ** K: key (3.1.14)
data Key


highland_no_key       :: Abc Key
highland_no_key       = abcLiteral $ text "HP"

highland_mixolydian   :: Abc Key
highland_mixolydian   = abcLiteral $ text "Hp"

data KeySpec

key_spec :: Abc Elt_Note -> Abc Elt_Mode ->  Abc Key
key_spec (Abc n) (Abc m) = Abc $ sequenceL (<+>) [n,m] 
  
data KeyAccidental

key_sharp   :: Abc KeyAccidental
key_sharp   = abcLiteral $ char '#' 
  
  
key_flat    :: Abc KeyAccidental
key_flat    = abcLiteral $ char 'b'
  
data Elt_Mode

mode :: String -> Abc Elt_Mode
mode = abcLiteral . text

major, minor, lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian 
    :: Abc Elt_Mode 
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
-- * The tune body (4)

-- body appends a blank line to the end of the output
body :: Abc a -> Abc CT_MidTuneField
body a = abcSeq2 (<$>) a (abcLiteral empty)

data CT_Element

tune :: Abc CT_Element
tune = Abc $ sequenceS (<+>) emptyseq

-- ** Pitch (4.1)

-- Abc has pitches in a two octave range and then uses octave specs for higher
-- and lower octaves
data PitchLetter = C | D | E | F | G | A | B | C2 | D2 | E2 | F2 | G2 | A2 | B2
  deriving (Eq,Enum,Ord,Show) 

instance Pretty PitchLetter where
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
  
    
data Elt_Note

note          :: PitchLetter -> Abc Elt_Note
note          = abcLiteral . pretty
  
instance Append CT_Element Elt_Note

data Attr_Octave

octaveHigh    :: Int -> Abc Attr_Octave
octaveHigh i  = abcLiteral $ text (replicate i '\'')
  
  
octaveLow     :: Int -> Abc Attr_Octave
octaveLow i   = abcLiteral $ text (replicate i ',')  
  
  
instance SuffixAttr Elt_Note Attr_Octave

-- ** Accidentals (4.2)
data Attr_Accidental

natural       :: Abc Attr_Accidental
natural       = abcLiteral $ char '='

sharp         :: Abc Attr_Accidental
sharp         = abcLiteral $ char '^'
      
doubleSharp   :: Abc Attr_Accidental
doubleSharp   = abcLiteral $ string "^^"
      
flat          :: Abc Attr_Accidental
flat          = abcLiteral $ char '_'
      
doubleFlat    :: Abc Attr_Accidental
doubleFlat    = abcLiteral $ string "__"

instance PrefixAttr Elt_Note Attr_Accidental

      
-- ** Note lengths (4.3)
data Attr_Duration

dur :: MeterFraction -> Abc Attr_Duration
dur = abcLiteral . ppNoteLength 

instance SuffixAttr Elt_Note Attr_Duration
instance SuffixAttr Elt_Rest Attr_Duration

-- ** Broken rhythm (4.4)
data Elt_BrokenRhythm

-- '>' left note dotted, right note halved
dotted_left       :: Abc Elt_BrokenRhythm
dotted_left       = abcLiteral $ char '>'

dotted_leftn      :: Int -> Abc Elt_BrokenRhythm
dotted_leftn i    = abcLiteral $ text $ replicate i '>'
  
    
-- '<' left note halved, right note dotted 
dotted_right      :: Abc Elt_BrokenRhythm
dotted_right      = abcLiteral $ char '<'


dotted_rightn     :: Int -> Abc Elt_BrokenRhythm
dotted_rightn i   = abcLiteral $ text $ replicate i '<'
  
  
-- ** Rests (4.5)
data Elt_Rest

rest    :: Abc Elt_Rest
rest    = abcLiteral $ char 'z'

instance Append CT_Element Elt_Rest




-- ** Repeat \/ bar symbols & First and second repeats (4.8 & 4.9)
data Elt_RepeatMark

repeatMark :: String -> Abc Elt_RepeatMark
repeatMark = abcLiteral . text


instance Append CT_Element Elt_RepeatMark


-- ** Ties and slurs (4.11)  
data Elt_Tie

tie             :: Abc Elt_Tie
tie             = abcLiteral $ char '-'
  
instance Append CT_Element Elt_Tie

data Elt_Slur

beginSlur       :: Abc Elt_Slur
beginSlur       = abcLiteral $ lparen

endSlur         :: Abc Elt_Slur
endSlur         = abcLiteral $ rparen

instance Append CT_Element Elt_Slur


-- ** Grace notes (4.12)
data Elt_GraceNotes

gracenotes :: [Abc Elt_Note] -> Abc Elt_GraceNotes
gracenotes = Abc . nested lbrace rbrace . sequenceL (<>) . map unAbc


-- Its simpler if we make gracenotes a glyph rather than 
-- a prefix attr of a note.  
instance Append CT_Element Elt_GraceNotes

-- ** Duplets, triplets, quadruplets, etc. (4.13)
data Elt_NPlet

nplet   :: Int -> Abc Elt_NPlet
nplet   = Abc . literal . group . (char '(' <>) . int

instance Append CT_Element Elt_NPlet



-- ** Decorations (4.14)
data Attr_Decoration

tilde       :: Abc Attr_Decoration
tilde       = abcLiteral $ char '~' 
   
stacatto    :: Abc Attr_Decoration
stacatto    = abcLiteral $ char '.' 

downbow     :: Abc Attr_Decoration
downbow     = abcLiteral $ char 'v' 

upbow       :: Abc Attr_Decoration
upbow       = abcLiteral $ char 'u' 

instance PrefixAttr Elt_Note Attr_Decoration


-- ** Chords and unisons (4.17)
data Elt_Chord

chord :: [Abc Elt_Note] -> Abc Elt_Chord
chord = Abc . nested lbracket rbracket . sequenceL (<>) . map unAbc

instance Append CT_Element Elt_Chord

-- * Multiple voices (7)
-- ** Voice overlay (7.4)
  
-- type must be - Abc CT_Element -> Abc CT_Element  -> Abc CT_Element
-- for folding
  
(&\) :: Abc CT_Element -> Abc CT_Element  -> Abc CT_Element
(&\) _ _ = undefined   
  


-- * Named elements

-- Notes  
c_, d_, e_, f_, g_, a_, b_ :: Abc Elt_Note
c_  = note C
d_  = note D
e_  = note E
f_  = note F
g_  = note G
a_  = note A
b_  = note B

c__, d__, e__, f__, g__, a__, b__ ::  Abc Elt_Note
c__  = note C2
d__  = note D2
e__  = note E2
f__  = note F2
g__  = note G2
a__  = note A2
b__  = note B2

-- Rests

-- @z1@ - a rest of the default note length.
z1        :: Abc Elt_Rest
z1        = rest ! dur (1 // 1)

-- @z1@ - a rest of double the default note length.
z2        :: Abc Elt_Rest
z2        = rest ! dur (2 // 1)

-- @z4@ - a rest four times the default note length.
z4        :: Abc Elt_Rest
z4        = rest ! dur (2 // 1)

-- @z'2@ - a rest of half the default note length.
z'2       :: Abc Elt_Rest
z'2       = rest ! dur (1 // 2)

-- repeats and barlines

-- | @barline@ - single stroke @|@.
barline         :: Abc Elt_RepeatMark
barline         = repeatMark "|" 

-- | @thinThick@ - @|]@.
thinThick       :: Abc Elt_RepeatMark
thinThick       = repeatMark "|]" 

-- | @thickThin@ - @|]@.
thickThin       :: Abc Elt_RepeatMark
thickThin       = repeatMark "[|" 

-- | @beginRepeat@ - @|:@.
beginRepeat     :: Abc Elt_RepeatMark
beginRepeat     = repeatMark "|:"

-- | @endRepeat@ - @|:@.
endRepeat       :: Abc Elt_RepeatMark
endRepeat       = repeatMark ":|"

-- | @doubleRepeat@ - @::@.
doubleRepeat    :: Abc Elt_RepeatMark
doubleRepeat    = repeatMark "::"


-- | @firstRepeat@ - @[1@.
firstRepeat    :: Abc Elt_RepeatMark
firstRepeat     = repeatMark "[1"

-- | @secondRepeat@ - @[2@.
secondRepeat    :: Abc Elt_RepeatMark
secondRepeat    = repeatMark "[2"

-- | @firstEnding@ - @|1@.
firstEnding     :: Abc Elt_RepeatMark
firstEnding     = repeatMark "|1"


-- | @secondEnding@ - @:|2@.
secondEnding    :: Abc Elt_RepeatMark
secondEnding    = repeatMark ":|2"