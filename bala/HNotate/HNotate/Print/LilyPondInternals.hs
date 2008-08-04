{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             FlexibleContexts #-}

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

import HNotate.Print.OutputBase

import Data.Char
import Data.Monoid
import Data.Sequence ( (|>), (><) )
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

class Concat cxt

(>|<) :: (Concat cxt) => Ly cxt -> Ly cxt -> Ly cxt
(>|<) (Ly (Sequence op sa)) (Ly (Sequence _ sb)) = Ly $ Sequence op (sa >< sb)
(>|<) _                     _                    = error $
    "can't caten non sequences"



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

type LengthRep a = (a,a)

ppMeter :: Integral a => LengthRep a -> Doc
ppMeter (n,d) =
    group $ int (fromIntegral n) <> char '/' <> int (fromIntegral d)



--------------------------------------------------------------------------------

-- | Contexts
data LyCxt_ToplevelT
type LyCxt_Toplevel = Ly LyCxt_ToplevelT

toplevelStart       :: LyCxt_Toplevel
toplevelStart       = Ly $ sequenceS (<$>) mempty


-- | Properties inside header block e.g. title, dedication
data LyCxt_HeaderT
type LyCxt_Header = Ly LyCxt_HeaderT

headerStart         :: LyCxt_Header
headerStart         = Ly $ sequenceS (<$>) mempty


-- | Book context for multiple scores, markup
data LyCxt_BookT
type LyCxt_Book = Ly LyCxt_BookT


bookStart           :: LyCxt_Book
bookStart           = Ly $ sequenceS (<$>) mempty



-- | Glyphs - e.g. rest, note, skip etc
data LyCxt_ElementT
type LyCxt_Element = Ly LyCxt_ElementT

elementStart        :: LyCxt_Element
elementStart        = Ly $ sequenceS (</>) mempty


instance Concat LyCxt_ElementT


--------------------------------------------------------------------------------
-- ** Commenting input files (2.12)


data LyCmdVersionT
type LyCmdVersion = Ly LyCmdVersionT

version :: String -> LyCmdVersion
version = command1 "version" . lyLit . dquotes . text


instance Append LyCxt_ToplevelT LyCmdVersionT


data LyLineCommentT
type LyLineComment = Ly LyLineCommentT


lineComment :: String -> LyLineComment
lineComment s = lyLit $ text ('%':' ':s) <> linebreak


instance Append LyCxt_ToplevelT LyLineCommentT


data LyBlockCommentT
type LyBlockComment = Ly LyBlockCommentT

blockComment :: String -> LyBlockComment
blockComment s = lyLit $ string $ "%{ " ++ s ++ " %}"


instance Append LyCxt_ToplevelT LyBlockCommentT
instance Append LyCxt_ElementT LyBlockCommentT



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
pitch               = lyLit . pretty


data LyOctaveSpecT
type LyOctaveSpec = Ly LyOctaveSpecT

-- | Printed as @'@, @''@, @'''@, etc. - e.g. @c''@
raised              :: Int -> LyOctaveSpec
raised i            = lyLit $ text (replicate i '\'')


-- | Printed as @,@, @,,@, @,,,@, etc. - e.g. @d,,@
lowered             :: Int -> LyOctaveSpec
lowered i           = lyLit $ text  (replicate i ',')

instance SuffixAttr LyPitchT LyOctaveSpecT



data LyNoteT
type LyNote = Ly LyNoteT

note                :: LyPitch -> LyNote
note (Ly p)         = Ly p

instance Append LyCxt_ElementT LyNoteT



--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)
data LyAccidentalT
type LyAccidental = Ly LyAccidentalT

-- | Printed as @is@.
sharp               :: LyAccidental
sharp               = lyLit $ text "is"

-- | Printed as @es@.
flat                :: LyAccidental
flat                = lyLit $ text "es"

-- | Printed as @isis@.
doubleSharp         :: LyAccidental
doubleSharp         = lyLit $ text "isis"

-- | Printed as @eses@.
doubleFlat          :: LyAccidental
doubleFlat          = lyLit $ text "eses"


instance SuffixAttr LyPitchT LyAccidentalT

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

data LyCautionaryAccidentalT
type LyCautionaryAccidental = Ly LyCautionaryAccidentalT

-- | Printed as @!@.
reminder_accidental     :: LyCautionaryAccidental
reminder_accidental     = lyLit $ char '!'

-- | Printed as @?@.
cautionary_accidental   :: LyCautionaryAccidental
cautionary_accidental   = lyLit $ char '?'


instance SuffixAttr LyPitchT LyCautionaryAccidentalT

--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

data LyMicroToneT
type LyMicroTone = Ly LyMicroToneT

half_flat           :: LyMicroTone
half_flat           = lyLit $ string "ih"

half_sharp          :: LyMicroTone
half_sharp          = lyLit $ string "es"

instance SuffixAttr LyPitchT LyMicroToneT

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

data LyCmdRelativeT
type LyCmdRelative= Ly LyCmdRelativeT

-- | Printed as: \\relative c'' { ... expr ... }
relative            :: LyPitch -> Ly b -> LyCmdRelative
relative p e        =
    let bexpr = Ly $ nestedf bracesHanging (unLy e)
    in command2 "relative" p bexpr



instance Append LyCxt_ElementT LyCmdRelativeT


--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

data LyRestT
type LyRest = Ly LyRestT

rest                :: LyRest
rest                = lyLit $ char 'r'



instance Append LyCxt_ElementT LyRestT

--------------------------------------------------------------------------------
-- *** Skips (6.1.10)

-- \skip
data LyCmdSkipT
type LyCmdSkip = Ly LyCmdSkipT

skip                :: LyDuration -> LyCmdSkip
skip d              = lySeq2 (<+>) (cmd "skip") d

instance Append LyCxt_ElementT LyCmdSkipT

-- @spacer@ - "s1", "s2", etc
data LySkipT
type LySkip = Ly LySkipT

spacer              :: LySkip
spacer              = lyLit $ char 's'

instance Append LyCxt_ElementT LySkipT


--------------------------------------------------------------------------------
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)

data LyDurationT
type LyDuration = Ly LyDurationT

duration            :: Int -> LyDuration
duration            = lyLit . int

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
dotted i            = lyLit $ text (replicate i '.')

dot                 :: LyDotted
dot                 = lyLit $ char '.'

instance SuffixAttr LyDurationT LyDottedT



--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

data LyCmdTimesT
type LyCmdTimes = Ly LyCmdTimesT

times               :: Integral a => LengthRep a -> Ly e  -> LyCmdTimes
times r e           =
    let bexpr = Ly $ nestedf bracesSpaced (unLy e)
    in command2 "times" (lyLit $ ppMeter r) bexpr

instance Append LyCxt_ElementT LyCmdTimesT

--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)

data LyChordT
type LyChord = Ly LyChordT

chord               :: [LyPitch] -> LyChord
chord xs            =
    let notes = sequenceL (<+>) (map unLy xs)
    in Ly $ nestedf angles notes


-- to do - which instance is correct?
instance Append LyCxt_ElementT LyChordT
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

instance Append LyCxt_ElementT LyCmdStemT

--------------------------------------------------------------------------------
-- *** Basic polyphony (6.3.3)

-- Don't constrain the result type otherwise we can't fold (\\)

data LyPolyT
type LyPoly = Ly LyPolyT

openPoly            :: LyPoly
openPoly            = lyLit $ text "<< " <> line

closePoly           :: LyPoly
closePoly           = lyLit $ line <> text " >>"

(\\) :: Ly a -> Ly b -> Ly a
a \\ b                = lySeq3 (<>) a (lyLit $ text " \\\\" <> line) b



instance Append LyCxt_ElementT LyPolyT

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

data LyCmdClefT
type LyCmdClef = Ly LyCmdClefT

clef                :: LyClefType -> LyCmdClef
clef                = command1break "clef"

instance Append LyCxt_ElementT LyCmdClefT

data LyClefTypeT
type LyClefType = Ly LyClefTypeT

cleftype            :: String -> LyClefType
cleftype            = lyLit . text

-- for clef transpositions  make a cleftype with an doublequoted string

--------------------------------------------------------------------------------
-- *** Key signature (6.4.2)

data LyCmdKeyT
type LyCmdKey = Ly LyCmdKeyT

key                 :: LyPitch -> LyCmdKeyType -> LyCmdKey
key                 = command2break "key"

instance Append LyCxt_ElementT LyCmdKeyT


data LyCmdKeyTypeT
type LyCmdKeyType = Ly LyCmdKeyTypeT

keyType             :: String -> LyCmdKeyType
keyType             = cmd


--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

data LyCmdTimeT
type LyCmdTime = Ly LyCmdTimeT

time                :: Integral a => LengthRep a -> LyCmdTime
time                = command1 "time" . lyLit . ppMeter


instance Append LyCxt_ElementT LyCmdTimeT

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

data LyCmdBarT
type LyCmdBar = Ly LyCmdBarT

bar                 :: String -> LyCmdBar
bar                 = command1 "bar" . lyLit . text

instance Append LyCxt_ElementT LyCmdBarT

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

instance Append LyCxt_ElementT LyCmdCadenzaT


--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

data LyTieT
type LyTie = Ly LyTieT

-- | tie is printed as @~@.
tie                 :: LyTie
tie                 = lyLit $ char '~'

instance Append LyCxt_ElementT LyTieT

data LyCmdTieT
type LyCmdTie = Ly LyCmdTieT

cmdTie              :: String -> LyCmdTie
cmdTie              = cmd

instance Append LyCxt_ElementT LyCmdTieT

--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

data LySlurT
type LySlur = Ly LySlurT

openSlur            :: LySlur
openSlur            = lyLit $ char '('

closeSlur           :: LySlur
closeSlur           = lyLit $ char ')'


instance Append LyCxt_ElementT LySlurT


data LyCmdSlurT
type LyCmdSlur = Ly LyCmdSlurT

cmdSlur             :: String -> LyCmdSlur
cmdSlur             = cmd


instance Append LyCxt_ElementT LyCmdSlurT

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
openBeam            = lyLit $ char '['


closeBeam           :: LyBeam
closeBeam           = lyLit $ char ']'

instance Append LyCxt_ElementT LyBeamT

--------------------------------------------------------------------------------
-- *** Grace notes (6.5.7)

--

data LyCmdGraceT
type LyCmdGrace = Ly LyCmdGraceT

cmdGrace            :: String -> Ly a -> LyCmdGrace
cmdGrace            = command1

instance Append LyCxt_ElementT LyCmdGraceT

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
articulation        = lyLit . text


instance SuffixAttr LyNoteT LyArticulationT



data LyVerticalPlacementT
type LyVerticalPlacement = Ly LyVerticalPlacementT

aboveLit            :: LyVerticalPlacement
aboveLit            = lyLit $ char '^'

belowLit            :: LyVerticalPlacement
belowLit            = lyLit $ char '_'

defaultLit          :: LyVerticalPlacement
defaultLit          = lyLit $ char '-'


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
fingering           = lyLit . text . (:) '-' . show

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

instance Append LyCxt_ElementT LyCmdBreatheT

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

instance Append LyCxt_ElementT LyCmdAutochangeT -- not really correct


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
chordmode e         = command1 "chordmode" (bracesSpacedExpr e)

-- to do - instance of ?


--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

data LyCmdAddlyricsT
type LyCmdAddlyrics = Ly LyCmdAddlyricsT

addlyrics           :: String -> LyCmdAddlyrics
addlyrics s         = command1 "addlyrics" (lyLit $ string s)

-- {CONTEXT} ?

-- *** Melismata (7.3.5)
data LyCmdMelismataT
type LyCmdMelismata = Ly LyCmdMelismataT

melisma             :: LyCmdMelismata
melisma             = cmd "melisma"


melismaEnd          :: LyCmdMelismata
melismaEnd          = cmd "melismaEnd"

instance Append LyCxt_ElementT LyCmdMelismataT

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
drums e             = command1 "drums" (bracesHangingExpr e)

-- to do - instance of ?


data LyDrumPitchNameT
type LyDrumPitchName = Ly LyDrumPitchNameT

drumPitchName       :: String -> LyDrumPitchName
drumPitchName       = lyLit . text

-- to do - instance of ?

--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)

-- | stringnum corresponds to @\\@ in LilyPond.
data LyStringnumT
type LyStringnum = Ly LyStringnumT

stringnum           :: Int -> LyStringnum
stringnum i         = lyLit $ text $ '\\' : show i


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
                      in lyLit $ text str

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
textScript          = lyLit . dquotes . text

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
markup s            = command1 "markup" (bracesSpacedExpr $ lyLit $ string s)

-- instance of a context?


--------------------------------------------------------------------------------
-- ** Preparing parts (8.2)
-- *** Metronome marks (8.2.2)

data LyCmdTempoT
type LyCmdTempo = Ly LyCmdTempoT

tempo               :: LyDuration -> Int -> LyCmdTempo
tempo d i =
    let expr = Ly $ sequenceL (<>) [unLy d, literal $ equals, literal $ int i]
    in command1 "tempo" expr



instance Append LyCxt_ElementT LyCmdTempoT

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)
-- new is a binary command (type x music-expr)
class NewContextType a

data LyCmdNewT
type LyCmdNew = Ly LyCmdNewT

newContext          :: (NewContextType ct) => Ly ct -> Ly a -> LyCmdNew
newContext ct e     = command2 "new" ct (bracesHangingExpr e)



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



instance Append LyCxt_ToplevelT CmdScoreT
instance Append LyCxt_BookT CmdScoreT

data LyCmdBookT
type LyCmdBook = Ly LyCmdBookT

book                :: LyBlock -> LyCmdBook
book                = command1 "book"


instance Append LyCxt_ToplevelT LyCmdBookT

--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)

data LyCmdHeaderT
type LyCmdHeader = Ly LyCmdHeaderT

header              ::  Ly a -> LyCmdHeader
header              = command1 "header" . bracesHangingExpr



instance Append LyCxt_ToplevelT LyCmdHeaderT


data LyBlockT
type LyBlock = Ly LyBlockT

block               :: Ly a -> LyBlock
block               = bracesHangingExpr

blockS              :: Ly a -> LyBlock
blockS              = bracesSpacedExpr

instance Append LyCxt_ToplevelT LyBlockT
instance Append LyCxt_ElementT LyBlockT

--------------------------------------------------------------------------------
-- *** Creating titles (10.2.1)

data LyHeaderElementT
type LyHeaderElement = Ly LyHeaderElementT

headerElement       :: String -> String -> LyHeaderElement
headerElement n v   = equation n (lyLit $ dquotes $ text v)



instance Append LyCxt_HeaderT LyHeaderElementT

--------------------------------------------------------------------------------
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

data LyCmdMidiT
type LyCmdMidi = Ly LyCmdMidiT

midi                :: LyCmdMidi
midi                = cmd "midi"





