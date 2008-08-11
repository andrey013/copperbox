{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts,
             FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Print.OutputLyInternals
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

module HNotate.Print.LilyPondInternals where

import HNotate.Print.Base

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
bracesHanging d = lbrace <$> indent 2 (d <$> rbrace)



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
-- ** Commenting input files (2.12)


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





