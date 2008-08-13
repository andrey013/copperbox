{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, 
             FlexibleInstances, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  TextLilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Output LilyPond. 
-- Similar to Text.XHTML in the Hierarchical Libraries, but with some extra
-- typefulness due to the Ly phantom type.
--
--------------------------------------------------------------------------------

module TextLilyPond where

import CommonUtils

import Data.Char
import Data.Monoid
import Text.PrettyPrint.Leijen

-- A phantom type
newtype Ly a = Ly { getLy :: Doc }

instance WDoc Ly where
  unwrap (Ly a)   = a
  wrap a          = Ly a
  
  
instance Show (Ly a) where
  show (Ly a) = show $ pretty a






runLy (Ly a) = putDoc (pretty a)




-- | Prefix a command name with \\.
cmd :: String -> Ly o
cmd =  wrap . text . ('\\' :)

command1 :: String -> Ly a -> Ly o
command1 s a = caten (<+>) (cmd s) a

command2 :: String -> Ly a -> Ly b -> Ly o
command2 s a b = caten3 (<+>) (cmd s) a b

command1break :: String -> Ly a -> Ly o
command1break s a = caten3 (<+>) (cmd s) a (wrap linebreak)

command2break :: String -> Ly a -> Ly b -> Ly o
command2break s a b = caten4 (<+>) (cmd s) a b (wrap linebreak)


context :: String -> Ly o
context =  wrap . text

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
bracesHanging d = lbrace <$> indent 2 d <$> rbrace



equation :: String -> Ly a -> Ly o
equation var expr = wrap $ text var <+> equals <+> unwrap expr

type LengthRep a = (a,a)

ppMeter :: Integral a => LengthRep a -> Doc
ppMeter (n,d) =
    group $ int (fromIntegral n) <> char '/' <> int (fromIntegral d)



--------------------------------------------------------------------------------



-- | Contexts
data LyCxt_ToplevelT
type LyCxt_Toplevel = Ly LyCxt_ToplevelT

type LilyPondScore = LyCxt_Toplevel

toplevelStart       :: LyCxt_Toplevel
toplevelStart       = wrap empty

instance Sequence LyCxt_ToplevelT
instance Monoid (Ly LyCxt_ToplevelT) where
  mempty = toplevelStart
  mappend = caten (<$>)
  

-- | Properties inside header block e.g. title, dedication
data LyCxt_HeaderT
type LyCxt_Header = Ly LyCxt_HeaderT

headerStart         :: LyCxt_Header
headerStart         = wrap empty

instance Sequence LyCxt_HeaderT
instance Monoid (Ly LyCxt_HeaderT) where
  mempty = headerStart
  mappend = caten (<$>)
  
  
-- | Book context for multiple scores, markup
data LyCxt_BookT
type LyCxt_Book = Ly LyCxt_BookT


bookStart           :: LyCxt_Book
bookStart           = wrap empty

instance Sequence LyCxt_BookT
instance Monoid (Ly LyCxt_BookT) where
  mempty = bookStart
  mappend = caten (<$>)

-- | Glyphs - e.g. rest, note, skip etc
data LyCxt_ElementT
type LyCxt_Element = Ly LyCxt_ElementT

elementStart        :: LyCxt_Element
elementStart        = wrap empty

instance Sequence LyCxt_ElementT
instance Monoid (Ly LyCxt_ElementT) where
  mempty = elementStart
  mappend = caten (</>)
  
-- instance Concat LyCxt_ElementT


--------------------------------------------------------------------------------
-- ** Working on text files (2.1.3)


data LyCmdVersionT
type LyCmdVersion = Ly LyCmdVersionT

version :: String -> LyCmdVersion
version = command1 "version" . wrap . dquotes . text


instance Append Ly LyCxt_ToplevelT LyCmdVersionT


data LyLineCommentT
type LyLineComment = Ly LyLineCommentT


lineComment :: String -> LyLineComment
lineComment s = wrap $ text ('%':' ':s) <> linebreak


instance Append Ly LyCxt_ToplevelT LyLineCommentT


data LyBlockCommentT
type LyBlockComment = Ly LyBlockCommentT

blockComment :: String -> LyBlockComment
blockComment s = wrap $ string $ "%{ " ++ s ++ " %}"


instance Append Ly LyCxt_ToplevelT LyBlockCommentT
instance Append Ly LyCxt_ElementT LyBlockCommentT



--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)


data LyPitchName = C | D | E | F | G | A | B
  deriving (Eq,Enum,Ord,Show)

instance Pretty LyPitchName where
  pretty = text . map toLower . show

data LyPitchT
type LyPitch = Ly LyPitchT


-- | Printed as @c d e f g a b@
pitch               :: LyPitchName -> LyPitch
pitch               = wrap . pretty


data LyOctaveSpecT
type LyOctaveSpec = Ly LyOctaveSpecT

-- | Printed as @'@, @''@, @'''@, etc. - e.g. @c''@
raised              :: Int -> LyOctaveSpec
raised i            = wrap $ text (replicate i '\'')


-- | Printed as @,@, @,,@, @,,,@, etc. - e.g. @d,,@
lowered             :: Int -> LyOctaveSpec
lowered i           = wrap $ text  (replicate i ',')

instance SuffixAttr LyPitchT LyOctaveSpecT



data LyNoteT
type LyNote = Ly LyNoteT

note                :: LyPitch -> LyNote
note                = promote

instance Append Ly LyCxt_ElementT LyNoteT



--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)
data LyAccidentalT
type LyAccidental = Ly LyAccidentalT

-- | Printed as @is@.
sharp               :: LyAccidental
sharp               = wrap $ text "is"

-- | Printed as @es@.
flat                :: LyAccidental
flat                = wrap $ text "es"

-- | Printed as @isis@.
doubleSharp         :: LyAccidental
doubleSharp         = wrap $ text "isis"

-- | Printed as @eses@.
doubleFlat          :: LyAccidental
doubleFlat          = wrap $ text "eses"


instance SuffixAttr LyPitchT LyAccidentalT

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

data LyCautionaryAccidentalT
type LyCautionaryAccidental = Ly LyCautionaryAccidentalT

-- | Printed as @!@.
reminder_accidental     :: LyCautionaryAccidental
reminder_accidental     = wrap $ char '!'

-- | Printed as @?@.
cautionary_accidental   :: LyCautionaryAccidental
cautionary_accidental   = wrap $ char '?'


instance SuffixAttr LyPitchT LyCautionaryAccidentalT

--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

data LyMicroToneT
type LyMicroTone = Ly LyMicroToneT

half_flat           :: LyMicroTone
half_flat           = wrap $ string "ih"

half_sharp          :: LyMicroTone
half_sharp          = wrap $ string "es"

instance SuffixAttr LyPitchT LyMicroToneT

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

data LyCmdRelativeT
type LyCmdRelative= Ly LyCmdRelativeT

-- | Printed as: \\relative c'' { ... expr ... }
relative            :: LyPitch -> Ly b -> LyCmdRelative
relative p e        = command2 "relative" p (nested bracesHanging e)

instance Append Ly LyCxt_ElementT LyCmdRelativeT


--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

data LyRestT
type LyRest = Ly LyRestT

rest                :: LyRest
rest                = wrap $ char 'r'



instance Append Ly LyCxt_ElementT LyRestT

--------------------------------------------------------------------------------
-- *** Skips (6.1.10)

-- \skip
data LyCmdSkipT
type LyCmdSkip = Ly LyCmdSkipT

skip                :: LyDuration -> LyCmdSkip
skip d              = command1 "skip" d

instance Append Ly LyCxt_ElementT LyCmdSkipT

-- @spacer@ - "s1", "s2", etc
data LySkipT
type LySkip = Ly LySkipT

spacer              :: LySkip
spacer              = wrap $ char 's'

instance Append Ly LyCxt_ElementT LySkipT


--------------------------------------------------------------------------------
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)

data LyDurationT
type LyDuration = Ly LyDurationT

duration            :: Int -> LyDuration
duration            = wrap . int

-- shorthand
dur                :: Int -> LyDuration
dur                 = duration


-- | @\\longa@.
longa               :: LyDuration
longa               = cmd "longa"

-- | @\\breve@.
breve               :: LyDuration
breve               = cmd "breve"



instance SuffixAttr LyNoteT LyDurationT
instance SuffixAttr LyRestT LyDurationT
instance SuffixAttr LySkipT LyDurationT

--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)

data LyDottedT
type LyDotted = Ly LyDottedT

dotted              :: Int -> LyDotted
dotted i            = wrap $ text (replicate i '.')

dot                 :: LyDotted
dot                 = wrap $ char '.'

instance SuffixAttr LyDurationT LyDottedT



--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

data LyCmdTimesT
type LyCmdTimes = Ly LyCmdTimesT

times               :: Integral a => LengthRep a -> Ly e  -> LyCmdTimes
times r e           = command2 "times" (wrap $ ppMeter r) 
                                       (nested bracesSpaced e)


instance Append Ly LyCxt_ElementT LyCmdTimesT

--------------------------------------------------------------------------------
-- *** Bar check (6.2.5)

data LyBarCheckT
type LyBarCheck = Ly LyBarCheckT

barcheck            :: LyBarCheck
barcheck            = wrap $ char '|'

-- shorthand
bc                  :: LyBarCheck
bc                  = wrap $ char '|'


instance Append Ly LyCxt_ElementT LyBarCheckT

--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)

data LyChordT
type LyChord = Ly LyChordT

chord               :: [LyPitch] -> LyChord
chord               = wrap . angles . hsep . map unwrap



-- to do - which instance is correct?
instance Append Ly LyCxt_ElementT LyChordT
instance SuffixAttr LyChordT LyDurationT


--------------------------------------------------------------------------------
-- *** Stems (6.3.2)

data LyCmdStemT
type LyCmdStem = Ly LyCmdStemT

-- | @\\stemUp@.
stemUp              :: LyCmdStem
stemUp              = cmd "stemUp"

-- | @\\stemDown@.
stemDown            :: LyCmdStem
stemDown            = cmd "stemDown"

-- | @\\stemNeutral@.
stemNeutral         :: LyCmdStem
stemNeutral         = cmd "stemNeutral"

instance Append Ly LyCxt_ElementT LyCmdStemT

--------------------------------------------------------------------------------
-- *** Basic polyphony (6.3.3)

-- Don't constrain the result type otherwise we can't fold (\\)

data LyPolyT
type LyPoly = Ly LyPolyT

openPoly            :: LyPoly
openPoly            = wrap $ text "<< " <> line

closePoly           :: LyPoly
closePoly           = wrap $ line <> text " >>"

(\\) :: Ly a -> Ly b -> Ly a
a \\ b                = caten3 (<>) a (wrap $ text " \\\\" <> line) b



instance Append Ly LyCxt_ElementT LyPolyT

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

data LyCmdClefT
type LyCmdClef = Ly LyCmdClefT

clef                :: LyClefType -> LyCmdClef
clef                = command1break "clef"

instance Append Ly LyCxt_ElementT LyCmdClefT

data LyClefTypeT
type LyClefType = Ly LyClefTypeT

cleftype            :: String -> LyClefType
cleftype            = wrap . text

-- for clef transpositions  make a cleftype with an doublequoted string

--------------------------------------------------------------------------------
-- *** Key signature (6.4.2)

data LyCmdKeyT
type LyCmdKey = Ly LyCmdKeyT

key                 :: LyPitch -> LyCmdKeyType -> LyCmdKey
key                 = command2break "key"

instance Append Ly LyCxt_ElementT LyCmdKeyT


data LyCmdKeyTypeT
type LyCmdKeyType = Ly LyCmdKeyTypeT

keyType             :: String -> LyCmdKeyType
keyType             = cmd


--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

data LyCmdTimeT
type LyCmdTime = Ly LyCmdTimeT

time                :: Integral a => LengthRep a -> LyCmdTime
time                = command1break "time" . wrap . ppMeter


instance Append Ly LyCxt_ElementT LyCmdTimeT

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

data LyCmdBarT
type LyCmdBar = Ly LyCmdBarT

bar                 :: String -> LyCmdBar
bar                 = command1 "bar" . wrap . text

instance Append Ly LyCxt_ElementT LyCmdBarT

-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

data LyCmdCadenzaT
type LyCmdCadenza = Ly LyCmdCadenzaT

cadenzaOn           :: LyCmdCadenza
cadenzaOn           = cmd "cadenzaOn"

cadenzaOff          :: LyCmdCadenza
cadenzaOff          = cmd "cadenzaOff"

instance Append Ly LyCxt_ElementT LyCmdCadenzaT


--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

data LyTieT
type LyTie = Ly LyTieT

-- | tie is printed as @~@.
tie                 :: LyTie
tie                 = wrap $ char '~'

instance Append Ly LyCxt_ElementT LyTieT

data LyCmdTieT
type LyCmdTie = Ly LyCmdTieT

cmdTie              :: String -> LyCmdTie
cmdTie              = cmd

instance Append Ly LyCxt_ElementT LyCmdTieT

--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

data LySlurT
type LySlur = Ly LySlurT

openSlur            :: LySlur
openSlur            = wrap $ char '('

closeSlur           :: LySlur
closeSlur           = wrap $ char ')'


instance Append Ly LyCxt_ElementT LySlurT


data LyCmdSlurT
type LyCmdSlur = Ly LyCmdSlurT

cmdSlur             :: String -> LyCmdSlur
cmdSlur             = cmd


instance Append Ly LyCxt_ElementT LyCmdSlurT

--------------------------------------------------------------------------------
-- *** Phrasing slurs (6.5.3)
-- { ATTRIBUTE OF NOTE ? }

data LyCmdPhrasingSlurT
type LyCmdPhrasingSlur = Ly LyCmdPhrasingSlurT

cmdPhrasingSlur     :: String -> LyCmdPhrasingSlur
cmdPhrasingSlur     = cmd

instance SuffixAttr LyNoteT LyCmdPhrasingSlurT
instance SuffixAttr LyChordT LyCmdPhrasingSlurT

--------------------------------------------------------------------------------
-- *** Laissez vibrer ties (6.5.4)

data LyCmdLaissezVibrerT
type LyCmdLaissezVibrer = Ly LyCmdLaissezVibrerT

laissezVibrer       :: LyCmdLaissezVibrer
laissezVibrer       = cmd "laissezVibrer"

instance SuffixAttr LyNoteT LyCmdLaissezVibrerT
instance SuffixAttr LyChordT LyCmdLaissezVibrerT


--------------------------------------------------------------------------------
-- *** Automatic beams (6.5.5)
-- noBeam is a note attribute

data LyCmdNoBeamT
type LyCmdNoBeam = Ly LyCmdNoBeamT

noBeam              :: LyCmdNoBeam
noBeam              = cmd "noBeam"

instance SuffixAttr LyNoteT LyCmdNoBeamT
instance SuffixAttr LyChordT LyCmdNoBeamT

--------------------------------------------------------------------------------
-- *** Manual beams (6.5.6)

data LyBeamT
type LyBeam = Ly LyBeamT

openBeam            :: LyBeam
openBeam            = wrap $ char '['


closeBeam           :: LyBeam
closeBeam           = wrap $ char ']'

instance Append Ly LyCxt_ElementT LyBeamT

--------------------------------------------------------------------------------
-- *** Grace notes (6.5.7)

--

data LyCmdGraceT
type LyCmdGrace = Ly LyCmdGraceT

cmdGrace            :: String -> Ly a -> LyCmdGrace
cmdGrace            = command1

instance Append Ly LyCxt_ElementT LyCmdGraceT

--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1)

data LyCmdArticulationT
type LyCmdArticulation = Ly LyCmdArticulationT

cmdArticulation     :: String -> LyCmdArticulation
cmdArticulation     = cmd

instance SuffixAttr LyNoteT LyCmdArticulationT


data LyArticulationT
type LyArticulation = Ly LyArticulationT

articulation        :: String -> LyArticulation
articulation        = wrap . text


instance SuffixAttr LyNoteT LyArticulationT



data LyVerticalPlacementT
type LyVerticalPlacement = Ly LyVerticalPlacementT

aboveLit            :: LyVerticalPlacement
aboveLit            = wrap $ char '^'

belowLit            :: LyVerticalPlacement
belowLit            = wrap $ char '_'

defaultLit          :: LyVerticalPlacement
defaultLit          = wrap $ char '-'


above               :: (PrefixAttr elt LyVerticalPlacementT) => Ly elt -> Ly elt
above e             = aboveLit !> e

below               :: (PrefixAttr elt LyVerticalPlacementT) => Ly elt -> Ly elt
below e             = belowLit !> e

defaultPosition     :: (PrefixAttr elt LyVerticalPlacementT) => Ly elt -> Ly elt
defaultPosition e   = defaultLit !> e


instance PrefixAttr LyCmdArticulationT LyVerticalPlacementT
instance SuffixAttr LyFingeringT LyVerticalPlacementT


--------------------------------------------------------------------------------
-- *** Fingering instructions (6.6.2)

data LyFingeringT
type LyFingering = Ly LyFingeringT

fingering           :: Int -> LyFingering
fingering           = wrap . text . (:) '-' . show

instance SuffixAttr LyNoteT LyFingeringT

--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)


data LyCmdDynamicT
type LyCmdDynamic = Ly LyCmdDynamicT

cmdDynamic          :: String -> LyCmdDynamic
cmdDynamic          = cmd

instance SuffixAttr LyNoteT LyCmdDynamicT
instance SuffixAttr LyChordT LyCmdDynamicT


--------------------------------------------------------------------------------
-- *** Breath marks (6.6.4)


data LyCmdBreatheT
type LyCmdBreathe = Ly LyCmdBreatheT

breathe             :: LyCmdBreathe
breathe             = cmd "breathe"

instance Append Ly LyCxt_ElementT LyCmdBreatheT

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)

data LyCmdGlissandoT
type LyCmdGlissando = Ly LyCmdGlissandoT

glissando           :: LyCmdGlissando
glissando           = cmd "glissando"


instance SuffixAttr LyNoteT LyCmdGlissandoT
instance SuffixAttr LyChordT LyCmdGlissandoT



--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

data LyCmdArpeggioT
type LyCmdArpeggio = Ly LyCmdArpeggioT

arpeggio            :: LyCmdArpeggio
arpeggio            = cmd "arpeggio"

instance SuffixAttr LyChordT LyCmdArpeggioT


--------------------------------------------------------------------------------
-- *** Falls and doits (6.6.8)

data LyCmdBendAfterT
type LyCmdBendAfter = Ly LyCmdBendAfterT

bendAfter           :: LyCmdBendAfter
bendAfter           = cmd "bendAfter"


instance SuffixAttr LyNoteT LyCmdBendAfterT
instance SuffixAttr LyChordT LyCmdBendAfterT

--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

data LyCmdAutochangeT
type LyCmdAutochange = Ly LyCmdAutochangeT

autochange          :: LyCmdAutochange
autochange          = cmd "autochange"

instance Append Ly LyCxt_ElementT LyCmdAutochangeT -- not really correct


-- *** Pedals (7.1.2)
data LyCmdPedalT
type LyCmdPedal = Ly LyCmdPedalT

cmdPedal            :: String -> LyCmdPedal
cmdPedal            = cmd


instance SuffixAttr LyNoteT LyCmdPedalT
instance SuffixAttr LyChordT LyCmdPedalT

--------------------------------------------------------------------------------
-- ** Chord names (7.2)
-- *** Chords mode (7.2.2)

data LyCmdChordmodeT
type LyCmdChordmode = Ly LyCmdChordmodeT

chordmode           :: Ly a -> LyCmdChordmode
chordmode e         = command1 "chordmode" (nested bracesSpaced e)

-- to do - instance of ?


--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

data LyCmdAddlyricsT
type LyCmdAddlyrics = Ly LyCmdAddlyricsT

addlyrics           :: String -> LyCmdAddlyrics
addlyrics s         = command1 "addlyrics" (wrap $ string s)

-- {CONTEXT} ?

-- *** Melismata (7.3.5)
data LyCmdMelismataT
type LyCmdMelismata = Ly LyCmdMelismataT

melisma             :: LyCmdMelismata
melisma             = cmd "melisma"


melismaEnd          :: LyCmdMelismata
melismaEnd          = cmd "melismaEnd"

instance Append Ly LyCxt_ElementT LyCmdMelismataT

--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)
-- *** Showing melody rhythms (7.4.1)

data LyCxtRhythmicStaffT
type LyCxtRhythmicStaff = Ly LyCxtRhythmicStaffT

rhythmicStaff       :: LyCxtRhythmicStaff
rhythmicStaff       = context "RhythmicStaff"


instance NewContextType LyCxtRhythmicStaffT

--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

data LyCmdDrumsT
type LyCmdDrums = Ly LyCmdDrumsT

drums               :: Ly a -> LyCmdDrums
drums e             = command1 "drums" (nested bracesHanging e)

-- to do - instance of ?


data LyDrumPitchNameT
type LyDrumPitchName = Ly LyDrumPitchNameT

drumPitchName       :: String -> LyDrumPitchName
drumPitchName       = wrap . text

-- to do - instance of ?

--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)

-- | stringnum corresponds to @\\@ in LilyPond.
data LyStringnumT
type LyStringnum = Ly LyStringnumT

stringnum           :: Int -> LyStringnum
stringnum i         = wrap $ text $ '\\' : show i


instance SuffixAttr LyNoteT LyStringnumT

data LyCxtTabStaffT
type LyCxtTabStaff = Ly LyCxtTabStaffT

tabStaff            :: LyCxtTabStaff
tabStaff            = context "TabStaff"


instance NewContextType LyCxtTabStaffT

data LyCxtTabVoiceT
type LyCxtTabVoice = Ly LyCxtTabVoiceT

tabVoice            :: LyCxtTabVoice
tabVoice            = context "TabVoice"


instance NewContextType LyCxtTabVoiceT

-- *** Right hand fingerings (7.5.6)
data LyRightHandFingerT
type LyRightHandFinger = Ly LyRightHandFingerT

rightHandFinger     :: Int -> LyRightHandFinger
rightHandFinger i   = let str = "-\rightHandFinger #" ++ show i
                      in wrap $ text str

rh                  :: Int -> LyRightHandFinger
rh                  = rightHandFinger


instance SuffixAttr LyNoteT LyRightHandFingerT


--------------------------------------------------------------------------------
-- ** Other instrument specific notation (7.8)
-- *** Artificial harmonics (strings) (7.8.1)
data LyCmdHarmonicT
type LyCmdHarmonic = Ly LyCmdHarmonicT

harmonic            :: LyCmdHarmonic
harmonic            = cmd "harmonic"

instance SuffixAttr LyNoteT LyCmdHarmonicT

--------------------------------------------------------------------------------
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

data LyTextScriptT
type LyTextScript = Ly LyTextScriptT

textScript          :: String -> LyTextScript
textScript          = wrap . dquotes . text

instance SuffixAttr LyNoteT LyTextScriptT
instance SuffixAttr LyTextScriptT LyVerticalPlacementT

data LyCmdEmptyTextT
type LyCmdEmptyText = Ly LyCmdEmptyTextT

emptyText           :: LyCmdEmptyText
emptyText           = cmd "emptyText"

-- instance of a context?

data LyCmdFatTextT
type LyCmdFatText = Ly LyCmdFatTextT

fatText             :: LyCmdFatText
fatText             = cmd "fatText"

-- instance of a context?


-- *** Text markup (8.1.4)

data LyCmdMarkupT
type LyCmdMarkup = Ly LyCmdMarkupT

-- | Simplified for now - the body is a String.
markup              :: String -> LyCmdMarkup
markup s            = command1 "markup" (wrap $ bracesSpaced $ string s)

-- instance of a context?


--------------------------------------------------------------------------------
-- ** Preparing parts (8.2)
-- *** Metronome marks (8.2.2)

data LyCmdTempoT
type LyCmdTempo = Ly LyCmdTempoT

tempo               :: LyDuration -> Int -> LyCmdTempo
tempo d i           = command1break "tempo" (wrap $ unwrap d <> equals <> int i)



instance Append Ly LyCxt_ElementT LyCmdTempoT

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)
-- new is a binary command (type x music-expr)
class NewContextType a

data LyCmdNewT
type LyCmdNew = Ly LyCmdNewT

newContext          :: (NewContextType ct) => Ly ct -> Ly a -> LyCmdNew
newContext ct e     = command2 "new" ct (nested bracesHanging e)



data LyCxtStaffT
type LyCxtStaff = Ly LyCxtStaffT

staff               :: LyCxtStaff
staff               = context "Staff"

instance NewContextType LyCxtStaffT


data LyCxtVoiceT
type LyCxtVoice = Ly LyCxtVoiceT

voice               :: LyCxtVoice
voice               = context "Voice"

instance NewContextType LyCxtVoiceT

--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)
-- *** Multiple scores in a book (10.1.2)

data CmdScoreT
type LyCmdScore = Ly CmdScoreT

score               :: LyBlock -> LyCmdScore
score               = command1 "score"



instance Append Ly LyCxt_ToplevelT CmdScoreT
instance Append Ly LyCxt_BookT CmdScoreT

data LyCmdBookT
type LyCmdBook = Ly LyCmdBookT

book                :: LyBlock -> LyCmdBook
book                = command1 "book"


instance Append Ly LyCxt_ToplevelT LyCmdBookT

--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)

data LyCmdHeaderT
type LyCmdHeader = Ly LyCmdHeaderT

header              ::  Ly a -> LyCmdHeader
header              = command1 "header" . nested bracesHanging



instance Append Ly LyCxt_ToplevelT LyCmdHeaderT


data LyBlockT
type LyBlock = Ly LyBlockT

block               :: Ly a -> LyBlock
block               = nested bracesHanging

blockS              :: Ly a -> LyBlock
blockS              = nested bracesSpaced

instance Append Ly LyCxt_ToplevelT LyBlockT
instance Append Ly LyCxt_ElementT LyBlockT

--------------------------------------------------------------------------------
-- *** Creating titles (10.2.1)

data LyHeaderElementT
type LyHeaderElement = Ly LyHeaderElementT

headerElement       :: String -> String -> LyHeaderElement
headerElement n v   = equation n (wrap $ dquotes $ text v)



instance Append Ly LyCxt_HeaderT LyHeaderElementT

--------------------------------------------------------------------------------
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

data LyCmdMidiT
type LyCmdMidi = Ly LyCmdMidiT

midi                :: LyCmdMidi
midi                = cmd "midi"



--------------------------------------------------------------------------------
-- Named elements


-- *** Elements - Normal pitches (6.1.1)

_c, _d, _e, _f, _g , _a, _b :: LyPitch
_c      = pitch C
_d      = pitch D
_e      = pitch E
_f      = pitch F
_g      = pitch G
_a      = pitch A
_b      = pitch B

-- *** Elements - Clef (6.4.1)
-- | @treble@.
treble              :: LyClefType
treble              = cleftype "treble"

-- | @alto@.
alto                :: LyClefType
alto                = cleftype "alto"

-- | @tenor@.
tenor               :: LyClefType
tenor               = cleftype "tenor"

-- | @bass@.
bass                :: LyClefType
bass                = cleftype "bass"

-- | @french@.
french              :: LyClefType
french              = cleftype "french"

-- | @soprano@.
soprano             :: LyClefType
soprano             = cleftype "soprano"

-- | @mezzosoprano@.
mezzosoprano        :: LyClefType
mezzosoprano        = cleftype "mezzosoprano"

-- | @baritone@.
baritone            :: LyClefType
baritone            = cleftype "baritone"

-- | @varbaritone@.
varbaritone         :: LyClefType
varbaritone         = cleftype "varbaritone"

-- | @subbass@.
subbass             :: LyClefType
subbass             = cleftype "subbass"

-- | @percussion@.
percussion          :: LyClefType
percussion          = cleftype "percussion"

-- | @tabClef@.
tabClef             :: LyClefType
tabClef             = cleftype "tabClef"


-- *** Elements - Key signature (6.4.2)

-- | @\\major@.
major               :: LyCmdKeyType
major               = keyType "major"

-- | @\\minor@.
minor               :: LyCmdKeyType
minor               = keyType "minor"

-- | @\\ionian@.
ionian              :: LyCmdKeyType
ionian              = keyType "ionian"

-- | @\\locrian@.
locrian             :: LyCmdKeyType
locrian             = keyType "locrian"

-- | @\\aeolian@.
aeolian             :: LyCmdKeyType
aeolian             = keyType "aeolian"

-- | @\\mixolydian@.
mixolydian          :: LyCmdKeyType
mixolydian          = keyType "mixolydian"

-- | @\\lydian@.
lydian              :: LyCmdKeyType
lydian              = keyType "lydian"

-- | @\\phrygian@.
phrygian            :: LyCmdKeyType
phrygian            = keyType "phrygian"

-- | @\\dorian@.
dorian              :: LyCmdKeyType
dorian              = keyType "dorian"


-- *** Elements - Ties (6.5.1)

-- | @\\repeatTie@.
repeatTie           :: LyCmdTie
repeatTie           = cmdTie "repeatTie"

-- | @\\tieUp@.
tieUp               :: LyCmdTie
tieUp               = cmdTie "tieUp"

-- | @\\tieDown@.
tieDown             :: LyCmdTie
tieDown             = cmdTie "tieDown"

-- | @\\tieNeutral@.
tieNeutral          :: LyCmdTie
tieNeutral          = cmdTie "tieNeutral"

-- | @\\tieDotted@.
tieDotted           :: LyCmdTie
tieDotted           = cmdTie "tieDotted"

-- | @\\tieDashed@.
tieDashed           :: LyCmdTie
tieDashed           = cmdTie "tieDashed"

-- | @\\tieSolid@.
tieSolid            :: LyCmdTie
tieSolid            = cmdTie "tieSolid"

-- *** Elements - Slurs (6.5.2)

-- | @\\slurUp@.
slurUp              :: LyCmdSlur
slurUp              = cmdSlur "slurUp"

-- | @\\slurDown@.
slurDown            :: LyCmdSlur
slurDown            = cmdSlur "slurDown"

-- | @\\slurNeutral@.
slurNeutral         :: LyCmdSlur
slurNeutral         = cmdSlur "slurNeutral"

-- | @\\slurDashed@.
slurDashed          :: LyCmdSlur
slurDashed          = cmdSlur "slurDashed"

-- | @\\slurDotted@.
slurDotted          :: LyCmdSlur
slurDotted          = cmdSlur "slurDotted"

-- | @\\slurSolid@.
slurSolid           :: LyCmdSlur
slurSolid           = cmdSlur "slurSolid"


-- *** Elements - Phrasing slurs (6.5.3)

-- | @\\(@.
openPhrasingSlur    :: LyCmdPhrasingSlur
openPhrasingSlur    = cmdPhrasingSlur "("

-- | @\\)@.
closePhrasingSlur   :: LyCmdPhrasingSlur
closePhrasingSlur   = cmdPhrasingSlur ")"

-- | @\\phrasingSlurUp@.
phrasingSlurUp      :: LyCmdPhrasingSlur
phrasingSlurUp      = cmdPhrasingSlur "phrasingSlurUp"

-- | @\\phrasingSlurDown@.
phrasingSlurDown    :: LyCmdPhrasingSlur
phrasingSlurDown    = cmdPhrasingSlur "phrasingSlurDown"

-- | @\\phrasingSlurNeutral@.
phrasingSlurNeutral :: LyCmdPhrasingSlur
phrasingSlurNeutral = cmdPhrasingSlur "phrasingSlurNeutral"

-- *** Elements - Grace notes (6.5.7)

-- | @\\grace@.
grace               :: Ly a -> LyCmdGrace
grace               = cmdGrace "grace"

-- | @\\acciaccatura@.
acciaccatura        :: Ly a -> LyCmdGrace
acciaccatura        = cmdGrace "acciaccatura"

-- | @\\appoggiatura@.
appoggiatura        :: Ly a -> LyCmdGrace
appoggiatura        = cmdGrace "appoggiatura"

-- *** Elements - Articulations (6.6.1)

-- | @-^@ - dashHat, aka @marcato@.
dashHat             :: LyArticulation
dashHat             = articulation "-^"

-- | @-+@ - dashPlus, aka @stopped@.
dashPlus            :: LyArticulation
dashPlus            = articulation "-+"

-- | @--@ - dashDash, aka @tenuto@.
dashDash            :: LyArticulation
dashDash            = articulation "--"

-- | @-|@ - dashBar, aka @staccatissimo@.
dashBar             :: LyArticulation
dashBar             = articulation "-|"

-- | @->@ - dashLarger, aka @accent@.
dashLarger          :: LyArticulation
dashLarger          = articulation "->"

-- | @-.@ - dashDot, aka @staccato@.
dashDot             :: LyArticulation
dashDot             = articulation "-."

-- | @-_@ - dashUnderscore, aka @portato@.
dashUnderscore      :: LyArticulation
dashUnderscore      = articulation "-_"



-- | @\\accent@.
accent              :: LyCmdArticulation
accent              = cmdArticulation "accent"

-- | @\\marcato@.
marcato             :: LyCmdArticulation
marcato             = cmdArticulation "marcato"

-- | @\\staccatissimo@.
staccatissimo       :: LyCmdArticulation
staccatissimo       = cmdArticulation "staccatissimo"

-- | @\\espressivo@.
espressivo          :: LyCmdArticulation
espressivo          = cmdArticulation "espressivo"

-- | @\\staccato@.
staccato            :: LyCmdArticulation
staccato            = cmdArticulation "staccato"

-- | @\\tenuto@.
tenuto              :: LyCmdArticulation
tenuto              = cmdArticulation "tenuto"

-- | @\\portato@.
portato             :: LyCmdArticulation
portato             = cmdArticulation "portato"

-- | @\\upbow@.
upbow               :: LyCmdArticulation
upbow               = cmdArticulation "upbow"

-- | @\\downbow@.
downbow             :: LyCmdArticulation
downbow             = cmdArticulation "downbow"

-- | @\\flageolet@.
flageolet           :: LyCmdArticulation
flageolet           = cmdArticulation "flageolet"

-- | @\\thumb@.
thumb               :: LyCmdArticulation
thumb               = cmdArticulation "thumb"

-- | @\\lheel@.
lheel               :: LyCmdArticulation
lheel               = cmdArticulation "lheel"

-- | @\\rheel@.
rheel               :: LyCmdArticulation
rheel               = cmdArticulation "rheel"

-- | @\\ltoe@.
ltoe                :: LyCmdArticulation
ltoe                = cmdArticulation "ltoe"

-- | @\\rtoe@.
rtoe                :: LyCmdArticulation
rtoe                = cmdArticulation "rtoe"

-- | @\\open@.
open                :: LyCmdArticulation
open                = cmdArticulation "open"

-- | @\\stopped@.
stopped             :: LyCmdArticulation
stopped             = cmdArticulation "stopped"

-- | @\\turn@.
turn                :: LyCmdArticulation
turn                = cmdArticulation "turn"

-- | @\\reverseturn@.
reverseturn         :: LyCmdArticulation
reverseturn         = cmdArticulation "reverseturn"

-- | @\\trill@.
trill               :: LyCmdArticulation
trill               = cmdArticulation "trill"

-- | @\\prall@.
prall               :: LyCmdArticulation
prall               = cmdArticulation "prall"

-- | @\\mordent@.
mordent             :: LyCmdArticulation
mordent             = cmdArticulation "mordent"

-- | @\\prallprall@.
prallprall          :: LyCmdArticulation
prallprall          = cmdArticulation "prallprall"

-- | @\\prallmordent@.
prallmordent        :: LyCmdArticulation
prallmordent        = cmdArticulation "prallmordent"

-- | @\\upprall@.
upprall             :: LyCmdArticulation
upprall             = cmdArticulation "upprall"

-- | @\\downprall@.
downprall           :: LyCmdArticulation
downprall           = cmdArticulation "downprall"

-- | @\\upmordent@.
upmordent           :: LyCmdArticulation
upmordent           = cmdArticulation "upmordent"

-- | @\\downmordent@.
downmordent         :: LyCmdArticulation
downmordent         = cmdArticulation "downmordent"

-- | @\\pralldown@.
pralldown           :: LyCmdArticulation
pralldown           = cmdArticulation "pralldown"

-- | @\\prallup@.
prallup             :: LyCmdArticulation
prallup             = cmdArticulation "prallup"

-- | @\\lineprall@.
lineprall           :: LyCmdArticulation
lineprall           = cmdArticulation "lineprall"

-- | @\\signumcongruentiae@.
signumcongruentiae  :: LyCmdArticulation
signumcongruentiae  = cmdArticulation "signumcongruentiae"

-- | @\\shortfermata@.
shortfermata        :: LyCmdArticulation
shortfermata        = cmdArticulation "shortfermata"

-- | @\\fermata@.
fermata             :: LyCmdArticulation
fermata             = cmdArticulation "fermata"

-- | @\\longfermata@.
longfermata         :: LyCmdArticulation
longfermata         = cmdArticulation "longfermata"

-- | @\\verylongfermata@.
verylongfermata     :: LyCmdArticulation
verylongfermata     = cmdArticulation "verylongfermata"

-- | @\\segno@.
segno               :: LyCmdArticulation
segno               = cmdArticulation "segno"

-- | @\\coda@.
coda                :: LyCmdArticulation
coda                = cmdArticulation "coda"

-- | @\\varcoda@.
varcoda             :: LyCmdArticulation
varcoda             = cmdArticulation "varcoda"

-- *** Elements - Dynamics (6.6.3)
-- use underscore suffix _ so we don't swallow up valuable identifiers in the
-- namespace

-- | @\\ppppp@.
ppppp_              :: LyCmdDynamic
ppppp_              = cmdDynamic "ppppp"

-- | @\\pppp@.
pppp_               :: LyCmdDynamic
pppp_               = cmdDynamic "pppp"

-- | @\\ppp@.
ppp_                :: LyCmdDynamic
ppp_                = cmdDynamic "ppp"

-- | @\\pp@.
pp_                 :: LyCmdDynamic
pp_                 = cmdDynamic "pp"

-- | @\\p@ - renamed piano.
piano               :: LyCmdDynamic
piano               = cmdDynamic "p"

-- | @\\mp@.
mp_                 :: LyCmdDynamic
mp_                 = cmdDynamic "mp"

-- | @\\mf@.
mf_                 :: LyCmdDynamic
mf_                 = cmdDynamic "mf"

-- | @\\f@ - renamed forte.
forte               :: LyCmdDynamic
forte               = cmdDynamic "f"

-- | @\\ff@.
ff_                 :: LyCmdDynamic
ff_                 = cmdDynamic "ff"

-- | @\\fff@.
fff_                :: LyCmdDynamic
fff_                = cmdDynamic "fff"

-- | @\\ffff@.
ffff_               :: LyCmdDynamic
ffff_               = cmdDynamic "ffff"

-- | @\\fp@.
fp_                 :: LyCmdDynamic
fp_                 = cmdDynamic "fp"

-- | @\\sf@.
sf_                 :: LyCmdDynamic
sf_                 = cmdDynamic "sf"

-- | @\\sff@.
sff_                :: LyCmdDynamic
sff_                = cmdDynamic "sff"

-- | @\\sp@.
sp_                 :: LyCmdDynamic
sp_                 = cmdDynamic "sp"

-- | @\\spp@.
spp_                :: LyCmdDynamic
spp_                = cmdDynamic "spp"

-- | @\\sfz@.
sfz_                :: LyCmdDynamic
sfz_                = cmdDynamic "sfz"

-- | @\\rfz@.
rfz_                :: LyCmdDynamic
rfz_                = cmdDynamic "rfz"

-- | @\\<@.
openCrescendo       :: LyCmdDynamic
openCrescendo       = cmdDynamic "<"

-- | @\\>@.
openDecrescendo     :: LyCmdDynamic
openDecrescendo     = cmdDynamic ">"

-- | @\\!@.
closeDynamic        :: LyCmdDynamic
closeDynamic        = cmdDynamic "!"

-- | @\\cr@ - alias of \\<.
cr_                 :: LyCmdDynamic
cr_                 = cmdDynamic "cr"

-- | @\\decr@ - alias of \\>.
decr_               :: LyCmdDynamic
decr_               = cmdDynamic "decr"

-- | @\\dynamicUp@.
dynamicUp           :: LyCmdDynamic
dynamicUp           = cmdDynamic "dynamicUp"

-- | @\\dynamicDown@.
dynamicDown         :: LyCmdDynamic
dynamicDown         = cmdDynamic "dynamicDown"

-- | @\\dynamicNeutral@.
dynamicNeutral      :: LyCmdDynamic
dynamicNeutral      = cmdDynamic "dynamicNeutral"

-- *** Elements - Pedals (7.1.2)
-- | @\\sustainDown@.
sustainDown         :: LyCmdPedal
sustainDown         = cmdPedal "sustainDown"

-- | @\\sustainUp@.
sustainUp           :: LyCmdPedal
sustainUp           = cmdPedal "sustainUp"

-- | @\\unaCorda@.
unaCorda            :: LyCmdPedal
unaCorda            = cmdPedal "unaCorda"

-- | @\\treCorde@.
treCorde            :: LyCmdPedal
treCorde            = cmdPedal "treCorde"

-- | @\\sostenutoDown@.
sostenutoDown       :: LyCmdPedal
sostenutoDown       = cmdPedal "sostenutoDown"

-- | @\\sostenutoUp@.
sostenutoUp         :: LyCmdPedal
sostenutoUp         = cmdPedal "sostenutoUp"


--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

-- | @acousticbassdrum@.
acousticbassdrum    :: LyDrumPitchName
acousticbassdrum    = drumPitchName "acousticbassdrum"

-- | @bassdrum@.
bassdrum            :: LyDrumPitchName
bassdrum            = drumPitchName "bassdrum"

-- | @hisidestick@.
hisidestick         :: LyDrumPitchName
hisidestick         = drumPitchName "hisidestick"

-- | @sidestick@.
sidestick           :: LyDrumPitchName
sidestick           = drumPitchName "sidestick"

-- | @losidestick@.
losidestick         :: LyDrumPitchName
losidestick         = drumPitchName "losidestick"

-- | @acousticsnare@.
acousticsnare       :: LyDrumPitchName
acousticsnare       = drumPitchName "acousticbassdrum"

-- | @snare@.
snare               :: LyDrumPitchName
snare               = drumPitchName "snare"

-- | @handclap@.
handclap            :: LyDrumPitchName
handclap            = drumPitchName "handclap"

-- | @electricsnare@.
electricsnare       :: LyDrumPitchName
electricsnare       = drumPitchName "electricsnare"

-- | @lowfloortom@.
lowfloortom         :: LyDrumPitchName
lowfloortom         = drumPitchName "lowfloortom"

-- | @closedhihat@.
closedhihat         :: LyDrumPitchName
closedhihat         = drumPitchName "closedhihat"

-- | @hihat@.
hihat               :: LyDrumPitchName
hihat               = drumPitchName "hihat"

-- | @highfloortom@.
highfloortom        :: LyDrumPitchName
highfloortom        = drumPitchName "highfloortom"

-- | @pedalhihat@.
pedalhihat          :: LyDrumPitchName
pedalhihat          = drumPitchName "pedalhihat"

-- | @lowtom@.
lowtom              :: LyDrumPitchName
lowtom              = drumPitchName "lowtom"

-- | @openhihat@.
openhihat           :: LyDrumPitchName
openhihat           = drumPitchName "openhihat"

-- | @halfopenhihat@.
halfopenhihat       :: LyDrumPitchName
halfopenhihat       = drumPitchName "halfopenhihat"

-- | @lowmidtom@.
lowmidtom           :: LyDrumPitchName
lowmidtom           = drumPitchName "lowmidtom"

-- | @himidtom@.
himidtom            :: LyDrumPitchName
himidtom            = drumPitchName "himidtom"

-- | @crashcymbala@.
crashcymbala        :: LyDrumPitchName
crashcymbala        = drumPitchName "crashcymbala"

-- | @crashcymbal@.
crashcymbal         :: LyDrumPitchName
crashcymbal         = drumPitchName "crashcymbal"

-- | @hightom@.
hightom             :: LyDrumPitchName
hightom             = drumPitchName "hightom"

-- | @ridecymbala@.

ridecymbala         :: LyDrumPitchName
ridecymbala         = drumPitchName "ridecymbala"

-- | @ridecymbal@.
ridecymbal          :: LyDrumPitchName
ridecymbal          = drumPitchName "ridecymbal"

-- | @chinesecymbal@.
chinesecymbal       :: LyDrumPitchName
chinesecymbal       = drumPitchName "chinesecymbal"

-- | @ridebell@.
ridebell            :: LyDrumPitchName
ridebell            = drumPitchName "ridebell"

-- | @tambourine@.
tambourine          :: LyDrumPitchName
tambourine          = drumPitchName "tambourine"

-- | @splashcymbal@.
splashcymbal        :: LyDrumPitchName
splashcymbal        = drumPitchName "splashcymbal"

-- | @cowbell@.
cowbell             :: LyDrumPitchName
cowbell             = drumPitchName "cowbell"

-- | @crashcymbalb@.
crashcymbalb        :: LyDrumPitchName
crashcymbalb        = drumPitchName "crashcymbalb"

-- | @vibraslap@.
vibraslap           :: LyDrumPitchName
vibraslap           = drumPitchName "vibraslap"

-- | @ridecymbalb@.
ridecymbalb         :: LyDrumPitchName
ridecymbalb         = drumPitchName "ridecymbalb"

-- | @mutehibongo@.
mutehibongo         :: LyDrumPitchName
mutehibongo         = drumPitchName "mutehibongo"

-- | @hibongo@.
hibongo             :: LyDrumPitchName
hibongo             = drumPitchName "hibongo"

-- | @openhibongo@.
openhibongo         :: LyDrumPitchName
openhibongo         = drumPitchName "openhibongo"

-- | @mutelobongo@.
mutelobongo         :: LyDrumPitchName
mutelobongo         = drumPitchName "mutelobongo"

-- | @lobongo@.
lobongo             :: LyDrumPitchName
lobongo             = drumPitchName "lobongo"

-- | @openlobongo@.
openlobongo         :: LyDrumPitchName
openlobongo         = drumPitchName "openlobongo"

-- | @mutehiconga@.
mutehiconga         :: LyDrumPitchName
mutehiconga         = drumPitchName "mutehiconga"

-- | @muteloconga@.
muteloconga         :: LyDrumPitchName
muteloconga         = drumPitchName "muteloconga"

-- | @openhiconga@.
openhiconga         :: LyDrumPitchName
openhiconga         = drumPitchName "openhiconga"

-- | @hiconga@.
hiconga             :: LyDrumPitchName
hiconga             = drumPitchName "hiconga"

-- | @openloconga@.
openloconga         :: LyDrumPitchName
openloconga         = drumPitchName "openloconga"

-- | @loconga@.
loconga             :: LyDrumPitchName
loconga             = drumPitchName "loconga"

-- | @hitimbale@.
hitimbale           :: LyDrumPitchName
hitimbale           = drumPitchName "hitimbale"

-- | @lotimbale@.
lotimbale           :: LyDrumPitchName
lotimbale           = drumPitchName "lotimbale"

-- | @hiagogo@.
hiagogo             :: LyDrumPitchName
hiagogo             = drumPitchName "hiagogo"

-- | @loagogo@.
loagogo             :: LyDrumPitchName
loagogo             = drumPitchName "loagogo"

-- | @cabasa@.
cabasa              :: LyDrumPitchName
cabasa              = drumPitchName "cabasa"

-- | @maracas@.
maracas             :: LyDrumPitchName
maracas             = drumPitchName "maracas"

-- | @shortwhistle@.
shortwhistle        :: LyDrumPitchName
shortwhistle        = drumPitchName "shortwhistle"

-- | @longwhistle@.
longwhistle         :: LyDrumPitchName
longwhistle         = drumPitchName "longwhistle"

-- | @shortguiro@.
shortguiro          :: LyDrumPitchName
shortguiro          = drumPitchName "shortguiro"

-- | @longguiro@.
longguiro           :: LyDrumPitchName
longguiro           = drumPitchName "longguiro"

-- | @guiro@.
guiro               :: LyDrumPitchName
guiro               = drumPitchName "guiro"

-- | @claves@.
claves              :: LyDrumPitchName
claves              = drumPitchName "claves"

-- | @hiwoodblock@.
hiwoodblock         :: LyDrumPitchName
hiwoodblock         = drumPitchName "hiwoodblock"

-- | @lowoodblock@.
lowoodblock         :: LyDrumPitchName
lowoodblock         = drumPitchName "lowoodblock"

-- | @mutecuica@.
mutecuica           :: LyDrumPitchName
mutecuica           = drumPitchName "mutecuica"

-- | @opencuica@.
opencuica           :: LyDrumPitchName
opencuica           = drumPitchName "opencuica"

-- | @mutetriangle@.
mutetriangle        :: LyDrumPitchName
mutetriangle        = drumPitchName "mutetriangle"

-- | @triangle@.
triangle            :: LyDrumPitchName
triangle            = drumPitchName "triangle"

-- | @opentriangle@.
opentriangle        :: LyDrumPitchName
opentriangle        = drumPitchName "opentriangle"

-- | @oneup@.
oneup               :: LyDrumPitchName
oneup               = drumPitchName "oneup"

-- | @twoup@.
twoup               :: LyDrumPitchName
twoup               = drumPitchName "twoup"

-- | @threeup@.
threeup             :: LyDrumPitchName
threeup             = drumPitchName "threeup"

-- | @fourup@.
fourup              :: LyDrumPitchName
fourup              = drumPitchName "fourup"

-- | @fiveup@.
fiveup              :: LyDrumPitchName
fiveup              = drumPitchName "fiveup"

-- | @onedown@.
onedown             :: LyDrumPitchName
onedown             = drumPitchName "onedown"

-- | @twodown@.
twodown             :: LyDrumPitchName
twodown             = drumPitchName "twodown"

-- | @threedown@.
threedown           :: LyDrumPitchName
threedown           = drumPitchName "threedown"

-- | @fourdown@.
fourdown            :: LyDrumPitchName
fourdown            = drumPitchName "fourdown"

-- | @fivedown@.
fivedown            :: LyDrumPitchName
fivedown            = drumPitchName "fivedown"



-- | @bda@ - abbreviated name for 'acousticbassdrum'.
bda                 :: LyDrumPitchName
bda                 = drumPitchName "bda"

-- | @bd@ - abbreviated name for 'bassdrum'.
bd                  :: LyDrumPitchName
bd                  = drumPitchName "bd"

-- | @ssh@ - abbreviated name for 'hisidestick'.
ssh                 :: LyDrumPitchName
ssh                 = drumPitchName "ssh"

-- | @ss@ - abbreviated name for 'sidestick'.
ss                  :: LyDrumPitchName
ss                  = drumPitchName "ss"

-- | @ssl@ - abbreviated name for 'losidestick'.
ssl                 :: LyDrumPitchName
ssl                 = drumPitchName "ssl"

-- | @sna@ - abbreviated name for 'acousticsnare'.
sna                 :: LyDrumPitchName
sna                 = drumPitchName "sna"

-- | @sn@ - abbreviated name for 'snare'.
sn                  :: LyDrumPitchName
sn                  = drumPitchName "sn"

-- | @hc@ - abbreviated name for 'handclap'.
hc                  :: LyDrumPitchName
hc                  = drumPitchName "hc"

-- | @sne@ - abbreviated name for 'electricsnare'.
sne                 :: LyDrumPitchName
sne                 = drumPitchName "sne"

-- | @tomfl@ - abbreviated name for 'lowfloortom'.
tomfl               :: LyDrumPitchName
tomfl               = drumPitchName "tomfl"

-- | @hhc@ - abbreviated name for 'closedhihat'.
hhc                 :: LyDrumPitchName
hhc                 = drumPitchName "hhc"

-- | @hh@ - abbreviated name for 'hihat'.
hh                  :: LyDrumPitchName
hh                  = drumPitchName "hh"

-- | @tomfh@ - abbreviated name for 'highfloortom'.
tomfh               :: LyDrumPitchName
tomfh               = drumPitchName "tomfh"


-- | @hhp@ - abbreviated name for 'pedalhihat'.
hhp                 :: LyDrumPitchName
hhp                 = drumPitchName "hhp"

-- | @toml@ - abbreviated name for 'lowtom'.
toml                :: LyDrumPitchName
toml                = drumPitchName "toml"

-- | @hho@ - abbreviated name for 'openhihat'.
hho                 :: LyDrumPitchName
hho                 = drumPitchName "hho"

-- | @hhho@ - abbreviated name for 'halfopenhihat'.
hhho                :: LyDrumPitchName
hhho                = drumPitchName "hhho"

-- | @tomml@ - abbreviated name for 'lowmidtom'.
tomml               :: LyDrumPitchName
tomml               = drumPitchName "tomml"

-- | @tommh@ - abbreviated name for 'himidtom'.
tommh               :: LyDrumPitchName
tommh               = drumPitchName "tommh"

-- | @cymca@ - abbreviated name for 'crashcymbala'.
cymca               :: LyDrumPitchName
cymca               = drumPitchName "cymca"

-- | @cymc@ - abbreviated name for 'crashcymbal'.
cymc                :: LyDrumPitchName
cymc                = drumPitchName "cymc"

-- | @tomh@ - abbreviated name for 'hightom'.
tomh                :: LyDrumPitchName
tomh                = drumPitchName "tomh"

-- | @cymra@ - abbreviated name for 'ridecymbala'.
cymra               :: LyDrumPitchName
cymra               = drumPitchName "cymra"

-- | @cymr@ - abbreviated name for 'ridecymbal'.
cymr                :: LyDrumPitchName
cymr                = drumPitchName "cymr"

-- | @cymch@ - abbreviated name for 'chinesecymbal'.
cymch               :: LyDrumPitchName
cymch               = drumPitchName "cymch"

-- | @rb@ - abbreviated name for 'ridebell'.
rb                  :: LyDrumPitchName
rb                  = drumPitchName "rb"

-- | @tamb@ - abbreviated name for 'tambourine'.
tamb                :: LyDrumPitchName
tamb                = drumPitchName "tamb"

-- | @cyms@ - abbreviated name for 'splashcymbal'.
cyms                :: LyDrumPitchName
cyms                = drumPitchName "cyms"

-- | @cb@ - abbreviated name for 'cowbell'.
cb                  :: LyDrumPitchName
cb                  = drumPitchName "cb"

-- | @cymcb@ - abbreviated name for 'crashcymbalb'.
cymcb               :: LyDrumPitchName
cymcb               = drumPitchName "cymcb"

-- | @vibs@ - abbreviated name for 'vibraslap'.
vibs                :: LyDrumPitchName
vibs                = drumPitchName "vibs"

-- | @cymrb@ - abbreviated name for 'ridecymbalb'.
cymrb               :: LyDrumPitchName
cymrb               = drumPitchName "cymrb"

-- | @bohm@ - abbreviated name for 'mutehibongo'.
bohm                :: LyDrumPitchName
bohm                = drumPitchName "bohm"

-- | @boh@ - abbreviated name for 'hibongo'.
boh                 :: LyDrumPitchName
boh                 = drumPitchName "boh"

-- | @boho@ - abbreviated name for 'openhibongo'.
boho                :: LyDrumPitchName
boho                = drumPitchName "boho"

-- | @bolm@ - abbreviated name for 'mutelobongo'.
bolm                :: LyDrumPitchName
bolm                = drumPitchName "bolm"

-- | @bol@ - abbreviated name for 'lobongo'.
bol                 :: LyDrumPitchName
bol                 = drumPitchName "bol"

-- | @bolo@ - abbreviated name for 'openlobongo'.
bolo                :: LyDrumPitchName
bolo                = drumPitchName "bolo"

-- | @cghm@ - abbreviated name for 'mutehiconga'.
cghm                :: LyDrumPitchName
cghm                = drumPitchName "cghm"

-- | @cglm@ - abbreviated name for 'muteloconga'.
cglm                :: LyDrumPitchName
cglm                = drumPitchName "cglm"

-- | @cgho@ - abbreviated name for 'openhiconga'.
cgho                :: LyDrumPitchName
cgho                = drumPitchName "cgho"

-- | @cgh@ - abbreviated name for 'hiconga'.
cgh                 :: LyDrumPitchName
cgh                 = drumPitchName "cgh"

-- | @cglo@ - abbreviated name for 'openloconga'.
cglo                :: LyDrumPitchName
cglo                = drumPitchName "cglo"

-- | @cgl@ - abbreviated name for 'loconga'.
cgl                 :: LyDrumPitchName
cgl                 = drumPitchName "cgl"

-- | @timh@ - abbreviated name for 'hitimbale'.
timh                :: LyDrumPitchName
timh                = drumPitchName "timh"

-- | @timl@ - abbreviated name for 'lotimbale'.
timl                :: LyDrumPitchName
timl                = drumPitchName "timl"

-- | @agh@ - abbreviated name for 'hiagogo'.
agh                 :: LyDrumPitchName
agh                 = drumPitchName "agh"

-- | @agl@ - abbreviated name for 'loagogo'.
agl                 :: LyDrumPitchName
agl                 = drumPitchName "agl"

-- | @cab@ - abbreviated name for 'cabasa'.
cab                 :: LyDrumPitchName
cab                 = drumPitchName "cab"

-- | @mar@ - abbreviated name for 'maracas'.
mar                 :: LyDrumPitchName
mar                 = drumPitchName "mar"

-- | @whs@ - abbreviated name for 'shortwhistle'.
whs                 :: LyDrumPitchName
whs                 = drumPitchName "whs"

-- | @whl@ - abbreviated name for 'longwhistle'.
whl                 :: LyDrumPitchName
whl                 = drumPitchName "whl"

-- | @guis@ - abbreviated name for 'shortguiro'.
guis                :: LyDrumPitchName
guis                = drumPitchName "guis"

-- | @guil@ - abbreviated name for 'longguiro'.
guil                :: LyDrumPitchName
guil                = drumPitchName "guil"

-- | @gui@ - abbreviated name for 'guiro'.
gui                 :: LyDrumPitchName
gui                 = drumPitchName "gui"

-- | @cl@ - abbreviated name for 'claves'.
cl                  :: LyDrumPitchName
cl                  = drumPitchName "cl"

-- | @wbh@ - abbreviated name for 'hiwoodblock'.
wbh                 :: LyDrumPitchName
wbh                 = drumPitchName "wbh"

-- | @wbl@ - abbreviated name for 'lowoodblock'.
wbl                 :: LyDrumPitchName
wbl                 = drumPitchName "wbl"

-- | @cuim@ - abbreviated name for 'mutecuica'.
cuim                :: LyDrumPitchName
cuim                = drumPitchName "cuim"

-- | @cuio@ - abbreviated name for 'opencuica'.
cuio                :: LyDrumPitchName
cuio                = drumPitchName "cuio"


-- | @trim@ - abbreviated name for 'mutetriangle'.
trim                :: LyDrumPitchName
trim                = drumPitchName "trim"

-- | @tri@ - abbreviated name for 'triangle'.
tri                 :: LyDrumPitchName
tri                 = drumPitchName "tri"

-- | @trio@ - abbreviated name for 'opentriangle'.
trio                :: LyDrumPitchName
trio                = drumPitchName "trio"

-- | @tt@ - abbreviated name for 'tamtam'.
tt                  :: LyDrumPitchName
tt                  = drumPitchName "tt"

-- | @ua@ - abbreviated name for 'oneup'.
ua                  :: LyDrumPitchName
ua                  = drumPitchName "ua"

-- | @ub@ - abbreviated name for 'twoup'.
ub                  :: LyDrumPitchName
ub                  = drumPitchName "ub"

-- | @uc@ - abbreviated name for 'threeup'.
uc                  :: LyDrumPitchName
uc                  = drumPitchName "uc"

-- | @ud@ - abbreviated name for 'fourup'.
ud                  :: LyDrumPitchName
ud                  = drumPitchName "ud"

-- | @ue@ - abbreviated name for 'fiveup'.
ue                  :: LyDrumPitchName
ue                  = drumPitchName "ue"

-- | @da@ - abbreviated name for 'onedown'.
da                  :: LyDrumPitchName
da                  = drumPitchName "da"

-- | @db@ - abbreviated name for 'twodown'.
db                  :: LyDrumPitchName
db                  = drumPitchName "db"

-- | @dc@ - abbreviated name for 'threedown'.
dc                  :: LyDrumPitchName
dc                  = drumPitchName "dc"

-- | @dd@ - abbreviated name for 'fourdown'.
dd                  :: LyDrumPitchName
dd                  = drumPitchName "dd"

-- | @de@ - abbreviated name for 'fivedown'.
de                  :: LyDrumPitchName
de                  = drumPitchName "de"


--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)
-- *** Creating titles (10.2.1)

-- | @dedication@.
dedication          :: String -> LyHeaderElement
dedication          = headerElement "dedication"

-- | @title@.
title               :: String -> LyHeaderElement
title               = headerElement "title"

-- | @subtitle@.
subtitle            :: String -> LyHeaderElement
subtitle            = headerElement "subtitle"

-- | @subsubtitle@.
subsubtitle         :: String -> LyHeaderElement
subsubtitle         = headerElement "subsubtitle"

-- | @poet@.
poet                :: String -> LyHeaderElement
poet                = headerElement "poet"

-- | @composer@.
composer            :: String -> LyHeaderElement
composer            = headerElement "composer"

-- | @meter@.
meter               :: String -> LyHeaderElement
meter               = headerElement "meter"

-- | @opus@.
opus                :: String -> LyHeaderElement
opus                = headerElement "opus"

-- | @arranger@.
arranger            :: String -> LyHeaderElement
arranger            = headerElement "arranger"

-- | @instrument@.
instrument          :: String -> LyHeaderElement
instrument          = headerElement "arranger"

-- | @piece@.
piece               :: String -> LyHeaderElement
piece               = headerElement "piece"


-- | @copyright@.
copyright           :: String -> LyHeaderElement
copyright           = headerElement "copyright"

-- | @tagline@.
tagline             :: String -> LyHeaderElement
tagline             = headerElement "tagline"

-- | @breakbefore@.
breakbefore         :: Bool -> LyHeaderElement
breakbefore True    = equation "breakbefore" (wrap $ text "##t")
breakbefore False   = equation "breakbefore" (wrap $ text "##f")





