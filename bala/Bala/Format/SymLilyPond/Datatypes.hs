{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Datatypes for a subset of LilyPond format in the final-tagless 
-- (Symantics) style of Carette, Kiselyov, and Shan.
--
--------------------------------------------------------------------------------



module Bala.Format.SymLilyPond.Datatypes  where

import Bala.Format.Base.SymBase


-- * Prefix Attributes
-- | Normally the P (pretty-print interpretation) prints all attributes as a 
-- suffix to their element. We meed a special case for prefix attributes 
-- (e.g. vertical placement).
class PrefixAttribute elt attrib
class CPrefixAttr repr where
  prefixAttr  :: PrefixAttribute elt att =>  repr att -> repr elt -> repr elt
       
       
       
       
-- | Contexts
data CT_Toplevel

-- | Properties inside header block e.g. title, dedication
data CT_Header  

-- | Book context for multiple scores, markup
data CT_Book


-- | Glyphs - e.g. rest, note, skip etc
data CT_Element 
 
--------------------------------------------------------------------------------
-- ** Commenting input files (2.12)


data CmdVersion
class CCmdVersion repr where 
  version :: String -> repr CmdVersion

instance ListContext CT_Toplevel CmdVersion  
  

data LineComment
class CLineComment repr where
  lineComment :: String -> repr LineComment

instance ListContext CT_Toplevel LineComment


data BlockComment
class CBlockComment repr where
  blockComment :: String -> repr BlockComment

instance ListContext CT_Toplevel BlockComment

instance ListContext CT_Element BlockComment


--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)


data PitchName = C | D | E | F | G | A | B 
  deriving (Eq,Enum,Ord,Show)

data Pitch
class CPitch repr where
  -- | Printed as @c d e f g a b@
  pitch :: PitchName -> repr Pitch


  
data OctaveSpec
class COctaveSpec repr where  
  -- | Printed as @'@, @''@, @'''@, etc. - e.g. @c''@
  raised      :: Int -> repr OctaveSpec
  -- | Printed as @,@, @,,@, @,,,@, etc. - e.g. @d,,@
  lowered     :: Int -> repr OctaveSpec
  
instance Attribute Pitch OctaveSpec

-- is this wise? It would make things a bit more polymorphic
-- instance AttrOctaveSpec Note   
              
data Note
class CNote repr where
  note :: repr Pitch -> repr Note

instance ListContext CT_Element Note
 
--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)  
data Accidental
class CAccidental repr where
  -- | Printed as @is@.
  sharp       :: repr Accidental
  -- | Printed as @es@.
  flat        :: repr Accidental
  -- | Printed as @isis@.
  doubleSharp :: repr Accidental
  -- | Printed as @eses@.
  doubleFlat  :: repr Accidental
  
  
instance Attribute Pitch Accidental

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

data CautionaryAccidental
class CCautionaryAccidental repr where
  -- | Printed as @!@.
  reminderAccidental    :: repr CautionaryAccidental
                        
  -- | Printed as @?@.
  cautionaryAccidental  :: repr CautionaryAccidental
                           
instance Attribute Pitch CautionaryAccidental 
                           
--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

data MicroTone
class CMicroTone repr where
  halfFlat  :: repr MicroTone
  halfSharp :: repr MicroTone
  
instance Attribute Pitch MicroTone

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

data CmdRelative
class CCmdRelative repr where
  -- | Printed as: \\relative c'' { ... expr ... }
  relative :: repr Pitch -> repr b -> repr CmdRelative

instance ListContext CT_Element CmdRelative
  
--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

data Rest
class CRest repr where
  rest :: repr Rest

instance ListContext CT_Element Rest

--------------------------------------------------------------------------------  
-- *** Skips (6.1.10)

-- \skip
data CmdSkip
class CCmdSkip repr where
  cmdSkip :: String -> repr CmdSkip
  
-- "s1", "s2", eyc  
data SkipDuration
class CSkipDuration repr where
  skipDuration :: repr Duration -> repr SkipDuration

--------------------------------------------------------------------------------  
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)

data Duration
class CDuration repr where
  duration  :: Int -> repr Duration

-- Design choice - breve and longa could be members of SymDuration
-- In lilypond dotting works on them as per normal durations, and
-- it would give them the same time - useful if we have a function
-- that builds durations e.g. f :: a -> repr Duration
--
--  breve     :: repr Duration
--  longa     :: repr Duration

   
instance Attribute Rest Duration
instance Attribute Note Duration
instance Attribute Chord Duration



data CmdLongDuration
class CCmdLongDuration repr where
  cmdLongDuration :: String -> repr CmdLongDuration

instance Attribute Rest CmdLongDuration
instance Attribute Note CmdLongDuration
instance Attribute Chord CmdLongDuration



--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)

data Dotted
class CDotted repr where
  dotted :: Int -> repr Dotted

instance Attribute Duration Dotted
instance Attribute CmdLongDuration Dotted

--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

data CmdTimes
class CCmdTimes repr where
  cmdTimes :: MeterFraction -> repr (SnocList ctx) -> repr CmdTimes


--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)
  
data Chord
-- Use '[repr Pitch]' as the list of pitches (not a SnocList)
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class CChord repr where
  chord :: [repr Pitch] -> repr Chord

instance ListContext CT_Element Chord
--------------------------------------------------------------------------------
-- *** Stems (6.3.2)

data CmdStem
class CCmdStem repr where
  cmdStem :: String -> repr CmdStem

--------------------------------------------------------------------------------
-- *** Basic polyphony (6.3.3)

-- Don't constrain the result type otherwise we can't fold (\\)

data Poly
class CPoly repr where
  openPoly    :: repr Poly
  closePoly   :: repr Poly
  (\\) :: repr a -> repr b -> repr a

instance ListContext CT_Element Poly


--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

data CmdClef
class CCmdClef repr where
  clef :: repr ClefType -> repr CmdClef

instance ListContext CT_Element CmdClef
 
data ClefType
class CClefType repr where
  cleftype :: String -> repr ClefType


data ClefTransposition
class CClefTransposition repr where  
  clefTransposition :: Int -> repr ClefTransposition


  
-- Unfortunately this has to be an attibute of Pitch rather than ClefType
-- because of the pretty printing rules 
instance Attribute Pitch ClefTransposition

--------------------------------------------------------------------------------
-- *** Key signature (6.4.2)

data CmdKey
class CCmdKey repr where
  key :: repr Pitch -> repr CmdKeyType -> repr CmdKey

instance ListContext CT_Element CmdKey


data CmdKeyType
class CCmdKeyType repr where
  keyType :: String -> repr CmdKeyType
  


--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

data CmdTime
class CCmdTime repr where
  time :: MeterFraction -> repr CmdTime

instance ListContext CT_Element CmdTime

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

data CmdBar
class CCmdBar repr where
  bar :: String -> repr CmdBar
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

data CmdCadenza
class CCmdCadenza repr where
  cmdCadenza     :: String -> repr CmdCadenza

    
  
--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

data Tie
class CTie repr where
  -- | tie is printed as @~@.
  tie :: repr Tie

data CmdTie
class CCmdTie repr where
  cmdTie :: String -> repr CmdTie



--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

data Slur
class CSlur repr where
  openSlur  :: repr Slur
  closeSlur :: repr Slur
  
data CmdSlur
class CCmdSlur repr where
  cmdSlur :: String -> repr CmdSlur

--------------------------------------------------------------------------------
-- *** Phrasing slurs (6.5.3)
-- { ATTRIBUTE OF NOTE ? }

data CmdPhrasingSlur
class CCmdPhrasingSlur repr where
  cmdPhrasingSlur   :: String -> repr CmdPhrasingSlur

--------------------------------------------------------------------------------
-- *** Laissez vibrer ties (6.5.4)

data CmdLaissezVibrer
class CCmdLaissezVibrer repr where
  laissezVibrer :: repr CmdLaissezVibrer

instance Attribute Note CmdLaissezVibrer
instance Attribute Chord CmdLaissezVibrer

--------------------------------------------------------------------------------
-- *** Automatic beams (6.5.5)
-- noBeam is a note attribute

data CmdNoBeam
class CCmdNoBeam repr where
  noBeam :: repr CmdNoBeam

instance ListContext CT_Element CmdNoBeam

instance Attribute Note CmdNoBeam

--------------------------------------------------------------------------------
-- *** Manual beams (6.5.6)

data Beam
class CBeam repr where
  openBeam  :: repr Beam 
  closeBeam :: repr Beam

instance ListContext CT_Element Beam

--------------------------------------------------------------------------------  
-- *** Grace notes (6.5.7)

data CmdGrace
class CCmdGrace repr where
  cmdGrace         :: String -> repr CmdGrace

instance ListContext CT_Element CmdGrace
  
--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1)

data CmdArticulation
class CCmdArticulation repr where
  cmdArticulation :: String -> repr CmdArticulation

instance Attribute Note CmdArticulation
instance PrefixAttribute CmdArticulation VerticalPlacement

data Articulation
class CArticulation repr where
  articulation :: String -> repr Articulation
  
instance Attribute Note Articulation


data VPlacement = VAbove | VBelow | VDefault

-- placement of an articulation, slur ...
data VerticalPlacement
class CVerticalPlacement repr where
  verticalPlacement   :: VPlacement -> repr VerticalPlacement



instance Attribute Fingering VerticalPlacement

 
    
--------------------------------------------------------------------------------
-- *** Fingering instructions (6.6.2)

data Fingering
class CFingering repr where
  fingering       :: Int -> repr Fingering

instance Attribute Note Fingering

--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)

-- so many dynamics that we parameterize the SymCmdDynamic signture with String
-- rather than have a seperate clause for each one
data CmdDynamic
class CCmdDynamic repr where
  cmdDynamic      :: String -> repr CmdDynamic
 
--------------------------------------------------------------------------------
-- *** Breath marks (6.6.4)


data CmdBreathe
class CCmdBreathe repr where
  cmdBreathe      :: String -> repr CmdBreathe

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)

data CmdGlissando
class CCmdGlissando repr where
  cmdGlissando :: String -> repr CmdGlissando
  

--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

data CmdArpeggio
class CCmdArpeggio repr where
  cmdArpeggio          :: String -> repr CmdArpeggio


--------------------------------------------------------------------------------
-- *** Falls and doits (6.6.8)

data CmdBendAfter
class CCmdBendAfter repr where
  bendAfter       :: repr CmdBendAfter

--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

data CmdAutochange
class CCmdAutochange repr where
  autochange      :: repr CmdAutochange

-- *** Pedals (7.1.2)
data CmdPedal
class CCmdPedal repr where
  cmdPedal :: String -> repr CmdPedal



instance Attribute Note CmdPedal
instance Attribute Chord CmdPedal

--------------------------------------------------------------------------------
-- ** Chord names (7.2)
-- *** Chords mode (7.2.2)

data CmdChordmode
class CCmdChordmode repr where
  chordmode       :: repr a -> repr CmdChordmode
      
--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

data CmdAddlyrics
class CCmdAddlyrics repr where
  addlyrics       :: String -> repr CmdChordmode

-- *** Melismata (7.3.5)
data Melismata
class CMelismata repr where
  melisma         :: repr Melismata
  melismaEnd      :: repr Melismata

--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)
-- *** Showing melody rhythms (7.4.1)

data CtxRhythmicStaff
class CCtxRhythmicStaff repr where
  rhythmicStaff     :: repr CtxRhythmicStaff
  
instance NewContextType CtxRhythmicStaff

--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

data CmdDrums
class CCmdDrums repr where
  drums           :: repr a -> repr CmdChordmode

data DrumPitchName  
class CDrumPitchName repr where
  drumPitchName   :: String -> repr DrumPitchName
  
  
--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)

-- | stringnum corresponds to @\\@ in LilyPond.
data Stringnum
class CStringnum repr where
  stringnum :: Int -> repr Stringnum
 
instance Attribute Note Stringnum

data CtxTabStaff
class CCtxTabStaff repr where
  tabStaff     :: repr CtxTabStaff
  
instance NewContextType CtxTabStaff

data CtxTabVoice
class CCtxTabVoice repr where
  tabVoice     :: repr CtxTabVoice
  
instance NewContextType CtxTabVoice

-- *** Right hand fingerings (7.5.6)
data RightHandFinger
class CRightHandFinger repr where
  rightHandFinger   :: Int -> repr RightHandFinger

instance Attribute Note RightHandFinger

--------------------------------------------------------------------------------
-- ** Other instrument specific notation (7.8)
-- *** Artificial harmonics (strings) (7.8.1)
data CmdHarmonic
class CCmdHarmonic repr where
  cmdHarmonic   :: repr CmdHarmonic

instance Attribute Note CmdHarmonic
  
--------------------------------------------------------------------------------
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

data TextScript
class CTextScript repr where
  textScript        :: String -> repr TextScript

data CmdFatText
class CCmdFatText repr where
  fatText     :: repr CmdFatText
  

-- *** Text markup (8.1.4)

data CmdMarkup
class CCmdMarkup repr where
  -- | Simplified for now - the body is a String.
  markup     :: String -> repr CmdFatText
  
  
--------------------------------------------------------------------------------
-- ** Preparing parts (8.2)
-- *** Metronome marks (8.2.2)

data CmdTempo
class CCmdTempo repr where
  tempo :: repr Duration -> Int -> repr CmdTempo

instance ListContext CT_Element CmdTempo

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)
-- new is a binary command (type x music-expr)

data CmdNew
class CCmdNew repr where
  newContext  :: (NewContextType ct) => repr ct -> repr a -> repr CmdNew


class NewContextType a

data CtxStaff
class CCtxStaff repr where
  staff       :: repr CtxStaff
  
instance NewContextType CtxStaff  

data CtxVoice
class CCtxVoice repr where
  voice       :: repr CtxVoice
  
instance NewContextType CtxVoice 







--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)
-- *** Multiple scores in a book (10.1.2)

data CmdScore
class CCmdScore repr where
  score :: repr Block -> repr CmdScore

instance ListContext CT_Toplevel CmdScore
instance ListContext CT_Book CmdScore

data CmdBook
class CCmdBook repr where
  book :: repr Block -> repr CmdBook
 
instance ListContext CT_Toplevel CmdBook

--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)

data CmdHeader
class CCmdHeader repr where
  header :: repr a -> repr CmdHeader 

instance ListContext CT_Toplevel CmdHeader


data Block
class CBlock repr where
  block         :: repr (SnocList ctx) -> repr Block

instance ListContext CT_Toplevel Block
instance ListContext CT_Element Block

--------------------------------------------------------------------------------    
-- *** Creating titles (10.2.1)

data HeaderElement
class CHeaderElement repr where
  headerElement :: String -> String -> repr HeaderElement
  breakbefore   :: Bool -> repr HeaderElement
 
        
instance ListContext CT_Header HeaderElement
  


     
--------------------------------------------------------------------------------    
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

data CmdMidi
class CCmdMidi repr where
  midi          :: repr CmdMidi
  
--------------------------------------------------------------------------------    
-- * Placeholder

data Placeholder
class CPlaceholder repr where
  undef          :: repr Placeholder
  
  

