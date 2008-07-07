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
class SymPrefixAttr repr where
  prefixAttr  :: PrefixAttribute elt att 
              =>  repr (att ctx_att) -> repr (elt ctx_elt) -> repr (elt ctx_elt)
       
       
       
       
-- | Contexts
data CT_Toplevel
data CT_Book

-- | Properties inside header block e.g. title, dedication
data CT_Header  

data CT_Note 
data CT_NoteAttr

-- | Glyphs - e.g. rest, note, skip etc
data CT_Element 
 
--------------------------------------------------------------------------------
-- ** Commenting input files (2.12)


data CmdVersion ctx
class SymCmdVersion repr where 
  version :: String -> repr (CmdVersion CT_Toplevel)
  
  

data LineComment ctx
class SymLineComment repr where
  lineComment :: String -> repr (LineComment ctx)

data BlockComment ctx
class SymBlockComment repr where
  blockComment :: String -> repr (BlockComment ctx)


--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)


data PitchName = C | D | E | F | G | A | B 
  deriving (Eq,Enum,Ord,Show)

data Pitch ctx  
class SymPitch repr where
  -- | Printed as @c d e f g a b@
  pitch :: PitchName -> repr (Pitch ctx) 


  
data OctaveSpec ctx
class SymOctaveSpec repr where  
  -- | Printed as @'@, @''@, @'''@, etc. - e.g. @c''@
  raised      :: Int -> repr (OctaveSpec ctx)
  -- | Printed as @,@, @,,@, @,,,@, etc. - e.g. @d,,@
  lowered     :: Int -> repr (OctaveSpec ctx)
  
instance Attribute Pitch OctaveSpec

-- is this wise? It would make things a bit more polymorphic
-- instance AttrOctaveSpec Note   
              
data Note ctx
class SymNote repr where
  note :: repr (Pitch ctx) -> repr (Note CT_Element)

 
--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)  
data Accidental ctx
class SymAccidental repr where
  -- | Printed as @is@.
  sharp       :: repr (Accidental ctx)
  -- | Printed as @es@.
  flat        :: repr (Accidental ctx)
  -- | Printed as @isis@.
  doubleSharp :: repr (Accidental ctx)
  -- | Printed as @eses@.
  doubleFlat  :: repr (Accidental ctx)
  
  
instance Attribute Pitch Accidental

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

data CautionaryAccidental ctx
class SymCautionaryAccidental repr where
  -- | Printed as @!@.
  reminderAccidental    :: repr (CautionaryAccidental ctx)
                        
  -- | Printed as @?@.
  cautionaryAccidental  :: repr (CautionaryAccidental ctx)
                           
instance Attribute Pitch CautionaryAccidental 
                           
--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

data MicroTone ctx
class SymMicroTone repr where
  halfFlat  :: repr (MicroTone ctx)
  halfSharp :: repr (MicroTone ctx)
  
instance Attribute Pitch MicroTone

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

data CmdRelative ctx
class SymCmdRelative repr where
  -- | Printed as: \\relative c'' { ... expr ... }
  relative :: repr (Pitch ctx) -> repr (b ctxa) -> repr (CmdRelative ctxb) 

  
--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

data Rest ctx
class SymRest repr where
  rest :: repr (Rest ctx)

--------------------------------------------------------------------------------  
-- *** Skips (6.1.10)

-- \skip
data CmdSkip ctx
class SymCmdSkip repr where
  cmdSkip :: String -> repr (CmdSkip ctx)  
  
-- "s1", "s2", eyc  
data SkipDuration ctx
class SymSkipDuration repr where
  skipDuration :: repr (Duration ctx) -> repr (SkipDuration ctx)

--------------------------------------------------------------------------------  
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)

data Duration ctx
class SymDuration repr where
  duration :: Int -> repr (Duration ctx)


   
instance Attribute Rest Duration
instance Attribute Note Duration
instance Attribute Chord Duration


data CmdLongDuration ctx
class SymCmdLongDuration repr where
  cmdLongDuration :: String -> repr (CmdLongDuration ctx)

instance Attribute Rest CmdLongDuration
instance Attribute Note CmdLongDuration
instance Attribute Chord CmdLongDuration

--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)

data Dotted ctx
class SymDotted repr where
  dotted :: Int -> repr (Dotted ctx)

instance Attribute Duration Dotted

--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

data CmdTimes ctx
class SymCmdTimes repr where
  cmdTimes :: MeterFraction -> repr (CList ctxa) -> repr (CmdTimes ctxb)


--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)
  
data Chord ctx
-- Use '[repr (Pitch ctx)]' as the list of pitches (not 'repr [repr (Pitch ctx)]')
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class SymChord repr where
  chord :: [repr (Pitch ctx)] -> repr (Chord ctx)

--------------------------------------------------------------------------------
-- *** Stems (6.3.2)

data CmdStem ctx
class SymCmdStem repr where
  cmdStem :: String -> repr (CmdStem ctx)

--------------------------------------------------------------------------------
-- *** Basic polyphony (6.3.3)

-- Don't constrain the result type otherwise we can't fold (\\)

class SymPoly repr where
  openPoly    :: repr (a ctx)
  closePoly   :: repr (a ctx)
  (\\) :: repr (a ctx) -> repr (a ctx) -> repr (a ctx)

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

data CmdClef ctx
class SymCmdClef repr where
  clef :: repr (ClefType ctx) -> repr (CmdClef CT_Element)

 
data ClefType ctx
class SymClefType repr where
  cleftype :: String -> repr (ClefType ctx)


data ClefTransposition ctx
class SymClefTransposition repr where  
  clefTransposition :: Int -> repr (ClefTransposition ctx) 


  
-- Unfortunately this has to be an attibute of Pitch rather than ClefType
-- because of the pretty printing rules 
instance Attribute Pitch ClefTransposition

--------------------------------------------------------------------------------
-- *** Key signature (6.4.2)

data CmdKey ctx
class SymCmdKey repr where
  key :: repr (Pitch ctx) -> repr (CmdKeyType CT_Element) -> repr (CmdKey CT_Element)

data CmdKeyType ctx
class SymCmdKeyType repr where
  keyType :: String -> repr (CmdKeyType CT_Element)
  

--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

data CmdTime ctx
class SymCmdTime repr where
  time :: MeterFraction -> repr (CmdTime CT_Element)

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

data CmdBar ctx
class SymCmdBar repr where
  bar :: String -> repr (CmdBar ctx)
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

data CmdCadenza ctx
class SymCmdCadenza repr where
  cmdCadenza     :: String -> repr (CmdCadenza ctx)

    
  
--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

data Tie ctx
class SymTie repr where
  -- | tie is printed as @~@.
  tie :: repr (Tie ctx)

data CmdTie ctx
class SymCmdTie repr where
  cmdTie :: String -> repr (CmdTie ctx)  



--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

data Slur ctx
class SymSlur repr where
  openSlur  :: repr (Slur ctx)
  closeSlur :: repr (Slur ctx)
  
data CmdSlur ctx
class SymCmdSlur repr where
  cmdSlur :: String -> repr (CmdSlur ctx)  

--------------------------------------------------------------------------------
-- *** Phrasing slurs (6.5.3)
-- { ATTRIBUTE OF NOTE ? }

data CmdPhrasingSlur ctx
class SymCmdPhrasingSlur repr where
  cmdPhrasingSlur             :: String -> repr (CmdPhrasingSlur ctx)

--------------------------------------------------------------------------------
-- *** Laissez vibrer ties (6.5.4)

data CmdLaissezVibrer ctx
class SymCmdLaissezVibrer repr where
  laissezVibrer :: repr (CmdLaissezVibrer ctx)

instance Attribute Note CmdLaissezVibrer
instance Attribute Chord CmdLaissezVibrer

--------------------------------------------------------------------------------
-- *** Automatic beams (6.5.5)
-- noBeam is a note attribute

data CmdNoBeam ctx
class SymCmdNoBeam repr where
  noBeam :: repr (CmdNoBeam ctx)

instance Attribute Note CmdNoBeam

--------------------------------------------------------------------------------
-- *** Manual beams (6.5.6)

data Beam ctx
class SymBeam repr where
  openBeam  :: repr (Beam ctx)
  closeBeam :: repr (Beam ctx)

--------------------------------------------------------------------------------  
-- *** Grace notes (6.5.7)

data CmdGrace ctx
class SymCmdGrace repr where
  cmdGrace         :: String -> repr (CmdGrace ctx)


  
--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1)

data CmdArticulation ctx
class SymCmdArticulation repr where
  cmdArticulation :: String -> repr (CmdArticulation ctx)

instance Attribute Note CmdArticulation
instance PrefixAttribute CmdArticulation VerticalPlacement

data Articulation ctx
class SymArticulation repr where
  articulation :: String -> repr (Articulation ctx)
  
instance Attribute Note Articulation


data VPlacement = VAbove | VBelow | VDefault

-- placement of an articulation, slur ...
data VerticalPlacement ctx
class SymVerticalPlacement repr where
  verticalPlacement   :: VPlacement -> repr (VerticalPlacement ctx)



instance Attribute Fingering VerticalPlacement

 
    
--------------------------------------------------------------------------------
-- *** Fingering instructions (6.6.2)

data Fingering ctx
class SymFingering repr where
  fingering       :: Int -> repr (Fingering ctx) 

instance Attribute Note Fingering

--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)

-- so many dynamics that we parameterize the SymCmdDynamic signture with String
-- rather than have a seperate clause for each one
data CmdDynamic ctx
class SymCmdDynamic repr where
  cmdDynamic      :: String -> repr (CmdDynamic ctx)
 
--------------------------------------------------------------------------------
-- *** Breath marks (6.6.4)


data CmdBreathe ctx
class SymCmdBreathe repr where
  cmdBreathe      :: String -> repr (CmdBreathe ctx) 

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)

data CmdGlissando ctx
class SymCmdGlissando repr where
  cmdGlissando :: String -> repr (CmdGlissando ctx)
  

--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

data CmdArpeggio ctx
class SymCmdArpeggio repr where
  cmdArpeggio          :: String -> repr (CmdArpeggio ctx)


--------------------------------------------------------------------------------
-- *** Falls and doits (6.6.8)

data CmdBendAfter ctx 
class SymCmdBendAfter repr where
  bendAfter       :: repr (CmdBendAfter CT_Note) 

--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

data CmdAutochange ctx 
class SymCmdAutochange repr where
  autochange      :: repr (CmdAutochange CT_Note) 

-- *** Pedals (7.1.2)
data CmdPedal ctx
class SymCmdPedal repr where
  cmdPedal :: String -> repr (CmdPedal ctx)



instance Attribute Note CmdPedal
instance Attribute Chord CmdPedal

--------------------------------------------------------------------------------
-- ** Chord names (7.2)
-- *** Chords mode (7.2.2)

data CmdChordmode ctx
class SymCmdChordmode repr where
  chordmode       :: repr (a ctx) -> repr (CmdChordmode ctx)
      
--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

data CmdAddlyrics ctx
class SymCmdAddlyrics repr where
  addlyrics       :: String -> repr (CmdChordmode ctx)

-- *** Melismata (7.3.5)
data Melismata ctx
class SymMelismata repr where
  melisma         :: repr (Melismata ctx)
  melismaEnd      :: repr (Melismata ctx)

--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)
-- *** Showing melody rhythms (7.4.1)

data CtxRhythmicStaff ctx
class SymCtxRhythmicStaff repr where
  rhythmicStaff     :: repr (CtxRhythmicStaff ctx)
  
instance ContextType CtxRhythmicStaff

--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

data CmdDrums ctx
class SymCmdDrums repr where
  drums           :: repr (a ctxa) -> repr (CmdChordmode ctxb)

data DrumPitchName ctx  
class SymDrumPitchName repr where
  drumPitchName   :: String -> repr (DrumPitchName ctx) 
  
  
--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)

-- | stringnum corresponds to @\\@ in LilyPond.
data Stringnum ctx
class SymStringnum repr where
  stringnum :: Int -> repr (Stringnum ctx)
 
instance Attribute Note Stringnum

data CtxTabStaff ctx
class SymCtxTabStaff repr where
  tabStaff     :: repr (CtxTabStaff ctx)
  
instance ContextType CtxTabStaff

data CtxTabVoice ctx
class SymCtxTabVoice repr where
  tabVoice     :: repr (CtxTabVoice ctx)
  
instance ContextType CtxTabVoice

-- *** Right hand fingerings (7.5.6)
data RightHandFinger ctx
class SymRightHandFinger repr where
  rightHandFinger   :: Int -> repr (RightHandFinger ctx)

instance Attribute Note RightHandFinger

--------------------------------------------------------------------------------
-- ** Other instrument specific notation (7.8)
-- *** Artificial harmonics (strings) (7.8.1)
data CmdHarmonic ctx
class SymCmdHarmonic repr where
  cmdHarmonic   :: repr (CmdHarmonic ctx)

instance Attribute Note CmdHarmonic
  
--------------------------------------------------------------------------------
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

data TextSript ctx
class SymTextSript repr where
  textSript        :: String -> repr (TextSript ctx) 

data CmdFatText ctx 
class SymCmdFatText repr where
  fatText     :: repr (CmdFatText ctx) 
  

-- *** Text markup (8.1.4)

data CmdMarkup ctx 
class SymCmdMarkup repr where
  -- | Simplified for now - the body is a String.
  markup     :: String -> repr (CmdFatText ctx) 
  
  
--------------------------------------------------------------------------------
-- ** Preparing parts (8.2)
-- *** Metronome marks (8.2.2)

data CmdTempo ctx
class SymCmdTempo repr where
  tempo :: repr (Duration ctx) -> Int -> repr (CmdTempo ctx)

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)
-- new is a binary command (type x music-expr)

data CmdNew ctx
class SymCmdNew repr where
  newContext  :: (ContextType ct) => repr (ct ctx) -> repr (a ctx') -> repr (CmdNew ctx'')


class ContextType a

data CtxStaff ctx
class SymCtxStaff repr where
  staff       :: repr (CtxStaff ctx)
  
instance ContextType CtxStaff  

data CtxVoice ctx
class SymCtxVoice repr where
  voice       :: repr (CtxVoice ctx)
  
instance ContextType CtxVoice 







--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)
-- *** Multiple scores in a book (10.1.2)

data CmdScore ctx
class SymCmdScore repr where
  score :: repr (a subctx) -> repr (CmdScore CT_Book)


data CmdBook ctx
class SymCmdBook repr where
  book :: repr (a subctx) -> repr (CmdBook CT_Toplevel)
 


--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)

data CmdHeader ctx
class SymCmdHeader repr where
  header :: repr (a CT_Header) -> repr (CmdHeader CT_Toplevel)  

data Block ctx
class SymBlock repr where
  block         :: repr (CList subctx) -> repr (Block superctx)

--------------------------------------------------------------------------------    
-- *** Creating titles (10.2.1)

data EqnTitle ctx
class SymEqnTitle repr where
  title         :: String -> repr (EqnTitle CT_Header)  
  
data EqnDedication ctx
class SymEqnDedication repr where
  dedication    :: String -> repr (EqnDedication CT_Header) 
  
     
--------------------------------------------------------------------------------    
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

data CmdMidi ctx
class SymCmdMidi repr where
  midi          :: repr (CmdMidi ctx)
  
--------------------------------------------------------------------------------    
-- * Placeholder

data Placeholder ctx
class SymPlaceholder repr where
  undef          :: repr (Placeholder ctx)
  
  

