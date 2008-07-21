{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Output.OutputLyInternals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Output combinators for LilyPond. 
-- Similar to Text.XHTML in the Hierarchical Libraries, but with some extra
-- typefulness due to the Ly phantom type.
--
--------------------------------------------------------------------------------

module Bala.Format.Output.LilyPondInternals where

import Bala.Format.Output.OutputBase
import Bala.Base.Meter

import Data.Char
import Data.Sequence ( (|>) )
import Text.PrettyPrint.Leijen

-- A phantom type
newtype Ly a = Ly { unLy :: Skeleton Doc }

instance Show (Ly a) where
  show (Ly a) = show $ pretty a


-- A type constrained add-right (|>)
class Append cxts cxta

infixl 5 +++

(+++) :: (Append cxts cxta) => Ly cxts -> Ly cxta -> Ly cxts
(+++) (Ly (Sequence op sq)) (Ly a) = Ly $ Sequence op (sq |> a)
(+++)  _                     _     = error "can't append to a non sequence"


class SuffixAttr cxte cxta

infixl 7 !

( ! ) :: (SuffixAttr cxte cxta) => Ly cxte -> Ly cxta -> Ly cxte
( ! ) (Ly e) (Ly a) = Ly $ Attr (<>) e a

class PrefixAttr cxte cxta

infixl 7 !>
( !> ) :: (PrefixAttr cxte cxta) => Ly cxta -> Ly cxte ->  Ly cxte
( !> ) (Ly a) (Ly e) = Ly $ Attr (flip (<>)) e a



runLy (Ly a) = run a

lyLit = Ly . literal

lyLinebreak :: Ly a
lyLinebreak = lyLit $ linebreak


lySeq2 :: Caten Doc -> Ly a -> Ly b -> Ly o
lySeq2 op (Ly a) (Ly b) = Ly $ sequenceL op [a,b]

lySeq3 :: Caten Doc -> Ly a -> Ly b -> Ly c -> Ly o
lySeq3 op (Ly a) (Ly b) (Ly c) = Ly $ sequenceL op [a,b,c]

lySeq4 :: Caten Doc -> Ly a -> Ly b -> Ly c -> Ly d -> Ly o
lySeq4 op (Ly a) (Ly b) (Ly c) (Ly d) = Ly $ sequenceL op [a,b,c,d]


-- | Prefix a command name with \\.
cmd :: String -> Ly o
cmd =  lyLit . text . ('\\' :)

command1 :: String -> Ly a -> Ly o
command1 s a = lySeq2 (<+>) (cmd s) a

command2 :: String -> Ly a -> Ly b -> Ly o 
command2 s a b = lySeq3 (<+>) (cmd s) a b

command1break :: String -> Ly a -> Ly o
command1break s a = lySeq3 (<+>) (cmd s) a lyLinebreak

command2break :: String -> Ly a -> Ly b -> Ly o 
command2break s a b = lySeq4 (<+>) (cmd s) a b lyLinebreak


context :: String -> Ly o
context =  lyLit . text

-- | Version of pprint's braces that inserts spaces between the braces 
-- e.g. @{ content }@. LilyPond can be sensitive to this. 
bracesSpaced :: Doc -> Doc
bracesSpaced = enclose (lbrace <> space) (space <> rbrace)

-- | Version of pprint's braces that hangs its content: 
-- 
-- > ... {
-- >   content
-- > }
bracesHanging :: Doc -> Doc
bracesHanging d = lbrace <$> indent 2 (d <$> rbrace)

bracesSpacedExpr :: Ly a -> Ly o
bracesSpacedExpr (Ly e) = Ly $ nestedf bracesSpaced e

bracesHangingExpr :: Ly a -> Ly o
bracesHangingExpr (Ly e) = Ly $ nestedf bracesHanging e


equation :: String -> Ly a -> Ly o
equation var d = lySeq3 (<+>) (lyLit $ text var) (lyLit $ equals) d



ppMeter :: MeterFraction -> Doc
ppMeter mf = let (n,d) = unMeterFraction mf in
    group $ int (fromIntegral n) <> char '/' <> int (fromIntegral d)
    
    

--------------------------------------------------------------------------------

-- | Contexts
data CT_Toplevel

toplevel            ::  Ly CT_Toplevel
toplevel            = Ly $ sequenceS (<$>) emptyseq


-- | Properties inside header block e.g. title, dedication
data CT_Header  

headerBlk           ::  Ly CT_Header
headerBlk           = Ly $ sequenceS (<$>) emptyseq


-- | Book context for multiple scores, markup
data CT_Book


-- | Glyphs - e.g. rest, note, skip etc
data CT_Element 

elementBlk          ::  Ly CT_Element
elementBlk          = Ly $ sequenceS (</>) emptyseq

--------------------------------------------------------------------------------
-- ** Commenting input files (2.12)


data CmdVersion
 
version :: String -> Ly CmdVersion
version = command1 "version" . lyLit . dquotes . text


instance Append CT_Toplevel CmdVersion  
  

data LineComment

lineComment :: String -> Ly LineComment
lineComment s = lyLit $ text ('%':' ':s) <> linebreak 


instance Append CT_Toplevel LineComment


data BlockComment

blockComment :: String -> Ly BlockComment
blockComment s = lyLit $ string $ "%{ " ++ s ++ " %}"


instance Append CT_Toplevel BlockComment
instance Append CT_Element BlockComment



--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)


data PitchName = C | D | E | F | G | A | B 
  deriving (Eq,Enum,Ord,Show)

instance Pretty PitchName where 
  pretty = text . map toLower . show

data Pitch

-- | Printed as @c d e f g a b@
pitch :: PitchName -> Ly Pitch
pitch = lyLit . pretty

  
data OctaveSpec
  
-- | Printed as @'@, @''@, @'''@, etc. - e.g. @c''@
raised      :: Int -> Ly OctaveSpec
raised i    = lyLit $ text (replicate i '\'')


-- | Printed as @,@, @,,@, @,,,@, etc. - e.g. @d,,@
lowered     :: Int -> Ly OctaveSpec
lowered i   = lyLit $ text  (replicate i ',')
  
instance SuffixAttr Pitch OctaveSpec


              
data Note

note :: Ly Pitch -> Ly Note
note (Ly p) = Ly p

instance Append CT_Element Note



--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)  
data Accidental

-- | Printed as @is@.
sharp               :: Ly Accidental
sharp               = lyLit $ text "is" 

-- | Printed as @es@.
flat                :: Ly Accidental
flat                = lyLit $ text "es"

-- | Printed as @isis@.
doubleSharp         :: Ly Accidental
doubleSharp         = lyLit $ text "isis"

-- | Printed as @eses@.
doubleFlat          :: Ly Accidental
doubleFlat          = lyLit $ text "eses"  

    
instance SuffixAttr Pitch Accidental

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

data CautionaryAccidental

-- | Printed as @!@.
reminder_accidental     :: Ly CautionaryAccidental
reminder_accidental     = lyLit $ char '!'

-- | Printed as @?@.
cautionary_accidental   :: Ly CautionaryAccidental
cautionary_accidental   = lyLit $ char '?'
  
                             
instance SuffixAttr Pitch CautionaryAccidental 

--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

data MicroTone

half_flat               :: Ly MicroTone
half_flat               = lyLit $ string "ih"

half_sharp              :: Ly MicroTone 
half_sharp              = lyLit $ string "es"
    
instance SuffixAttr Pitch MicroTone

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

data CmdRelative

-- | Printed as: \\relative c'' { ... expr ... }
relative :: Ly Pitch -> Ly b -> Ly CmdRelative
relative p e          = 
    let bexpr = Ly $ nestedf bracesHanging (unLy e) 
    in command2 "relative" p bexpr



instance Append CT_Element CmdRelative


--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

data Rest

rest :: Ly Rest
rest = lyLit $ char 'r'

instance Append CT_Element Rest

--------------------------------------------------------------------------------  
-- *** Skips (6.1.10)

-- \skip
data CmdSkip

skip :: Ly Duration -> Ly CmdSkip
skip d = lySeq2 (<+>) (cmd "skip") d

instance Append CT_Element CmdSkip
  
-- "s1", "s2", eyc  
data SkipDuration

skip_duration :: Ly Duration -> Ly SkipDuration
skip_duration d = lySeq2 (<>) (lyLit $ char 's') d
  
instance Append CT_Element SkipDuration


--------------------------------------------------------------------------------  
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)

data Duration

duration  :: Int -> Ly Duration
duration  = lyLit . int

dur :: Int -> Ly Duration
dur = duration

-- | @\\longa@.
longa   :: Ly Duration
longa   = cmd "longa"  

-- | @\\breve@.
breve   :: Ly Duration
breve   = cmd "breve"

   
instance SuffixAttr Rest Duration
instance SuffixAttr Note Duration







--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)

data Dotted

dotted                :: Int -> Ly Dotted
dotted i              = lyLit $ text (replicate i '.') 

dot                   :: Ly Dotted
dot                   = lyLit $ char '.'

instance SuffixAttr Duration Dotted



--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

data CmdTimes

times :: MeterFraction -> Ly e  -> Ly CmdTimes
times r e          = 
    let bexpr = Ly $ nestedf bracesSpaced (unLy e)
    in command2 "times" (lyLit $ ppMeter r) bexpr



--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)
  
data Chord

chord                 :: [Ly Pitch] -> Ly Chord
chord xs              = 
    let notes = sequenceL (<+>) (map unLy xs) 
    in Ly $ nestedf angles notes

instance Append CT_Element Chord
instance SuffixAttr Chord Duration


--------------------------------------------------------------------------------
-- *** Stems (6.3.2)

data CmdStem

-- | @\\stemUp@.
stemUp                  :: Ly CmdStem
stemUp                  = cmd "stemUp"  

-- | @\\stemDown@.
stemDown                :: Ly CmdStem
stemDown                = cmd "stemDown"    

-- | @\\stemNeutral@.
stemNeutral             :: Ly CmdStem
stemNeutral             = cmd "stemNeutral" 
  
instance Append CT_Element CmdStem

--------------------------------------------------------------------------------
-- *** Basic polyphony (6.3.3)

-- Don't constrain the result type otherwise we can't fold (\\)

data Poly

openPoly    :: Ly Poly
openPoly              = lyLit $ text "<< " <> line

closePoly   :: Ly Poly
closePoly             = lyLit $ line <> text " >>"

(\\) :: Ly a -> Ly b -> Ly a
a \\ b                = lySeq3 (<>) a (lyLit $ text " \\\\" <> line) b
  
  

instance Append CT_Element Poly

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

data CmdClef

clef :: Ly ClefType -> Ly CmdClef
clef = command1break "clef"

instance Append CT_Element CmdClef
 
data ClefType

cleftype :: String -> Ly ClefType
cleftype = lyLit . text 

-- for clef transpositions  make a cleftype with an doublequoted string

--------------------------------------------------------------------------------
-- *** Key signature (6.4.2)

data CmdKey

key :: Ly Pitch -> Ly CmdKeyType -> Ly CmdKey
key = command2break "key"

instance Append CT_Element CmdKey


data CmdKeyType

keyType :: String -> Ly CmdKeyType
keyType = cmd


--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

data CmdTime

time :: MeterFraction -> Ly CmdTime
time = command1 "time" . lyLit . ppMeter


instance Append CT_Element CmdTime

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

data CmdBar

bar :: String -> Ly CmdBar
bar = command1 "bar" . lyLit . text
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

data CmdCadenza

cadenzaOn           :: Ly CmdCadenza
cadenzaOn           = cmd "cadenzaOn"

cadenzaOff          :: Ly CmdCadenza 
cadenzaOff          = cmd "cadenzaOff"


--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

data Tie

-- | tie is printed as @~@.
tie :: Ly Tie
tie                   = lyLit $ char '~'

instance Append CT_Element Tie

data CmdTie

cmdTie :: String -> Ly CmdTie
cmdTie = cmd

instance Append CT_Element CmdTie

--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

data Slur

openSlur  :: Ly Slur
openSlur              = lyLit $ char '('

closeSlur :: Ly Slur
closeSlur             = lyLit $ char ')'


instance Append CT_Element Slur

  
data CmdSlur

cmdSlur :: String -> Ly CmdSlur
cmdSlur = cmd

  
instance Append CT_Element CmdSlur

--------------------------------------------------------------------------------
-- *** Phrasing slurs (6.5.3)
-- { ATTRIBUTE OF NOTE ? }

data CmdPhrasingSlur

cmdPhrasingSlur   :: String -> Ly CmdPhrasingSlur
cmdPhrasingSlur   = cmd
  
instance SuffixAttr Note CmdPhrasingSlur
instance SuffixAttr Chord CmdPhrasingSlur

--------------------------------------------------------------------------------
-- *** Laissez vibrer ties (6.5.4)

data CmdLaissezVibrer

laissezVibrer :: Ly CmdLaissezVibrer
laissezVibrer = cmd "laissezVibrer"

instance SuffixAttr Note CmdLaissezVibrer
instance SuffixAttr Chord CmdLaissezVibrer


--------------------------------------------------------------------------------
-- *** Automatic beams (6.5.5)
-- noBeam is a note attribute

data CmdNoBeam

noBeam :: Ly CmdNoBeam
noBeam  = cmd "noBeam"

instance SuffixAttr Note CmdNoBeam
instance SuffixAttr Chord CmdNoBeam

--------------------------------------------------------------------------------
-- *** Manual beams (6.5.6)

data Beam

openBeam  :: Ly Beam 
openBeam              = lyLit $ char '['
  

closeBeam :: Ly Beam
closeBeam             = lyLit $ char ']'

instance Append CT_Element Beam

--------------------------------------------------------------------------------  
-- *** Grace notes (6.5.7)

-- 

data CmdGrace

cmdGrace            :: String -> Ly a -> Ly CmdGrace
cmdGrace            = command1 

instance Append CT_Element CmdGrace

--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1)

data CmdArticulation

cmdArticulation :: String -> Ly CmdArticulation
cmdArticulation = cmd

instance SuffixAttr Note CmdArticulation


data Articulation

articulation :: String -> Ly Articulation
articulation = lyLit . text 


instance SuffixAttr Note Articulation



data VerticalPlacement

aboveLit            :: Ly VerticalPlacement
aboveLit            = lyLit $ char '^'

belowLit            :: Ly VerticalPlacement
belowLit            = lyLit $ char '_'

defaultLit          :: Ly VerticalPlacement
defaultLit          = lyLit $ char '-'


above               :: (PrefixAttr elt VerticalPlacement) => Ly elt -> Ly elt
above e             = aboveLit !> e

below               :: (PrefixAttr elt VerticalPlacement) => Ly elt -> Ly elt
below e             = belowLit !> e

defaultPosition :: (PrefixAttr elt VerticalPlacement) => Ly elt -> Ly elt
defaultPosition e = defaultLit !> e


instance PrefixAttr CmdArticulation VerticalPlacement
instance SuffixAttr Fingering VerticalPlacement


--------------------------------------------------------------------------------
-- *** Fingering instructions (6.6.2)

data Fingering

fingering       :: Int -> Ly Fingering
fingering       = lyLit . text . (:) '-' . show 
 
instance SuffixAttr Note Fingering

--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)


data CmdDynamic

cmdDynamic      :: String -> Ly CmdDynamic
cmdDynamic      = cmd

instance SuffixAttr Note CmdDynamic
instance SuffixAttr Chord CmdDynamic


--------------------------------------------------------------------------------
-- *** Breath marks (6.6.4)


data CmdBreathe

breathe             :: Ly CmdBreathe
breathe             = cmd "breathe"
  
instance Append CT_Element CmdBreathe

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)

data CmdGlissando

glissando           :: Ly CmdGlissando
glissando           = cmd "glissando"


instance SuffixAttr Note CmdGlissando
instance SuffixAttr Chord CmdGlissando



--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

data CmdArpeggio

arpeggio            :: Ly CmdArpeggio
arpeggio            = cmd "arpeggio"
 
instance SuffixAttr Chord CmdArpeggio


--------------------------------------------------------------------------------
-- *** Falls and doits (6.6.8)

data CmdBendAfter

bendAfter           :: Ly CmdBendAfter
bendAfter           = cmd "bendAfter"


instance SuffixAttr Note CmdBendAfter
instance SuffixAttr Chord CmdBendAfter

--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

data CmdAutochange

autochange          :: Ly CmdAutochange
autochange          = cmd "autochange"

instance Append CT_Element CmdAutochange -- not really correct


-- *** Pedals (7.1.2)
data CmdPedal

cmdPedal            :: String -> Ly CmdPedal
cmdPedal            = cmd


instance SuffixAttr Note CmdPedal
instance SuffixAttr Chord CmdPedal

--------------------------------------------------------------------------------
-- ** Chord names (7.2)
-- *** Chords mode (7.2.2)

data CmdChordmode

chordmode           :: Ly a -> Ly CmdChordmode
chordmode e         = command1 "chordmode" (bracesSpacedExpr e)


--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

data CmdAddlyrics

addlyrics           :: String -> Ly CmdChordmode
addlyrics s         = command1 "addlyrics" (lyLit $ string s)

-- {CONTEXT} ?

-- *** Melismata (7.3.5)
data CmdMelismata

melisma             :: Ly CmdMelismata
melisma             = cmd "melisma"


melismaEnd          :: Ly CmdMelismata
melismaEnd          = cmd "melismaEnd"
  
instance Append CT_Element CmdMelismata
  
--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)
-- *** Showing melody rhythms (7.4.1)

data CtxRhythmicStaff

rhythmicStaff     :: Ly CtxRhythmicStaff
rhythmicStaff     = context "RhythmicStaff"


instance NewContextType CtxRhythmicStaff

--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

data CmdDrums

drums               :: Ly a -> Ly CmdChordmode
drums e             = command1 "drums" (bracesHangingExpr e)


data DrumPitchName  

drumPitchName   :: String -> Ly DrumPitchName
drumPitchName   = lyLit . text
 
--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)

-- | stringnum corresponds to @\\@ in LilyPond.
data Stringnum

stringnum           :: Int -> Ly Stringnum
stringnum i         = lyLit $ text $ '\\' : show i


instance SuffixAttr Note Stringnum

data CtxTabStaff

tabStaff          :: Ly CtxTabStaff
tabStaff          = context "TabStaff"

  
instance NewContextType CtxTabStaff

data CtxTabVoice

tabVoice          :: Ly CtxTabVoice
tabVoice          = context "TabVoice"


instance NewContextType CtxTabVoice

-- *** Right hand fingerings (7.5.6)
data RightHandFinger

rightHandFinger     :: Int -> Ly RightHandFinger
rightHandFinger i   = let str = "-\rightHandFinger #" ++ show i
                      in lyLit $ text str

rH                  :: Int -> Ly RightHandFinger
rH                  = rightHandFinger
                         
                               
instance SuffixAttr Note RightHandFinger


--------------------------------------------------------------------------------
-- ** Other instrument specific notation (7.8)
-- *** Artificial harmonics (strings) (7.8.1)
data CmdHarmonic

harmonic            :: Ly CmdHarmonic
harmonic            = cmd "harmonic"

instance SuffixAttr Note CmdHarmonic

--------------------------------------------------------------------------------
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

data TextScript

textScript          :: String -> Ly TextScript
textScript          = lyLit . dquotes . text

instance SuffixAttr Note TextScript
instance SuffixAttr TextScript VerticalPlacement

data CmdEmptyText

emptyText           :: Ly CmdEmptyText
emptyText           = cmd "emptyText"

 
data CmdFatText

fatText             :: Ly CmdFatText
fatText             = cmd "fatText"

-- *** Text markup (8.1.4)

data CmdMarkup

-- | Simplified for now - the body is a String.
markup              :: String -> Ly CmdFatText
markup s            = command1 "markup" (bracesSpacedExpr $ lyLit $ string s)


--------------------------------------------------------------------------------
-- ** Preparing parts (8.2)
-- *** Metronome marks (8.2.2)

data CmdTempo

tempo         :: Ly Duration -> Int -> Ly CmdTempo
tempo d i     = 
    let expr = Ly $ sequenceL (<>) [unLy d, literal $ equals, literal $ int i]
    in command1 "tempo" expr



instance Append CT_Element CmdTempo

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)
-- new is a binary command (type x music-expr)
class NewContextType a

data CmdNew

newContext  :: (NewContextType ct) => Ly ct -> Ly a -> Ly CmdNew
newContext ct e   = command2 "new" ct (bracesHangingExpr e) 



data CtxStaff

staff               :: Ly CtxStaff
staff               = context "Staff"

instance NewContextType CtxStaff  


data CtxVoice

voice               :: Ly CtxVoice
voice               = context "Voice"

instance NewContextType CtxVoice 

--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)
-- *** Multiple scores in a book (10.1.2)

data CmdScore

score               :: Ly Block -> Ly CmdScore
score               = command1 "score"



instance Append CT_Toplevel CmdScore
instance Append CT_Book CmdScore

data CmdBook

book                :: Ly Block -> Ly CmdBook
book                = command1 "book"


instance Append CT_Toplevel CmdBook

--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)

data CmdHeader

header              ::  Ly CT_Header -> Ly CmdHeader 
header              = command1 "header" . bracesHangingExpr                    
 


instance Append CT_Toplevel CmdHeader


data Block

block               :: Ly a -> Ly Block
block               = bracesHangingExpr

blockS              :: Ly a -> Ly Block
blockS              = bracesSpacedExpr

instance Append CT_Toplevel Block
instance Append CT_Element Block

--------------------------------------------------------------------------------    
-- *** Creating titles (10.2.1)

data HeaderElement

headerElement           :: String -> String -> Ly HeaderElement
headerElement var val   = equation var (lyLit $ dquotes $ text val)




--------------------------------------------------------------------------------    
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

data CmdMidi

midi                :: Ly CmdMidi
midi                = cmd "midi"





  
        
instance Append CT_Header HeaderElement

