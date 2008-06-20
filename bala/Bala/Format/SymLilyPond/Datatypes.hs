{-# LANGUAGE EmptyDataDecls #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations
--
-- Datatypes for a subset of LilyPond format in the final-tagless 
-- (Symantics) style of Carette, Kiselyov, and Shan.
--
--------------------------------------------------------------------------------



module Bala.Format.SymLilyPond.Datatypes  where

import Bala.Format.Base.SymBase



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
  deriving (Eq,Show)

data Pitch ctx  
class SymPitch repr where
  pitch :: PitchName -> repr (Pitch ctx) 


  
class AttrOctaveSpec a
class SymAttrOctaveSpec repr where  
  raised      :: (AttrOctaveSpec a) => Int -> repr (a ctx) -> repr (a ctx)
  lowered     :: (AttrOctaveSpec a) => Int -> repr (a ctx) -> repr (a ctx)
  
instance AttrOctaveSpec Pitch

-- is this wise? It would make things a bit more polymorphic
-- instance AttrOctaveSpec Note   
              
data Note ctx
class SymNote repr where
  note :: repr (Pitch ctx) -> repr (Note CT_Element)

 
--------------------------------------------------------------------------------
-- *** Accidentals (6.1.2)  
class AttrAccidental a
class SymAttrAccidental repr where
  sharp       :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  flat        :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleSharp :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleFlat  :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  
  
instance AttrAccidental Pitch  

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)

data CautionaryAccidental ctx
class SymCautionaryAccidental repr where
  reminderAccidental    :: repr (CautionaryAccidental ctx)
  cautionaryAccidental  :: repr (CautionaryAccidental ctx)

--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)

class AttrMicroTone a
class SymAttrMicroTone repr where
  halfFlat  :: (AttrMicroTone a) => repr (a ctx) -> repr (a ctx)
  halfSharp :: (AttrMicroTone a) => repr (a ctx) -> repr (a ctx)
  
instance AttrMicroTone Pitch 

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

data CmdRelative ctx
class SymCmdReleative repr where
  relative :: repr (Pitch ctx) -> repr (b ctx) -> repr (CmdRelative CT_Note) 

  
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

class AttrDuration a
class SymAttrDuration repr where
  attrduration :: (AttrDuration a) => repr (Duration ctx) -> repr (a ctx) -> repr (a ctx)




   
instance AttrDuration Rest
instance AttrDuration Note
instance AttrDuration Chord


class AttrCmdLongDuration a
class SymAttrCmdLongDuration repr where
  cmdLongDuration :: (AttrCmdLongDuration a) => String -> repr (a ctx) -> repr (a ctx)

instance AttrCmdLongDuration Rest
instance AttrCmdLongDuration Note
instance AttrCmdLongDuration Chord

--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)

class AttrDotted a
class SymAttrDotted repr where
  dotted :: (AttrDotted a) => Int -> repr (a ctx) -> repr (a ctx)

instance AttrDotted Duration

--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)

data Times ctx
class SymTimes repr where
  times :: MeterFraction -> repr (a ctx) -> repr (Times ctx)


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

data PolyCat ctx
class SymPolyCat repr where
  (\\) :: repr (a ctx) -> repr (a ctx) -> repr (PolyCat ctx)

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

data CmdClef ctx
class SymCmdClef repr where
  clef :: repr (ClefType ctx) -> repr (CmdClef CT_Element)

 
data ClefType ctx
class SymClefType repr where
  cleftype :: String -> repr (ClefType ctx)


class AttrClefTransposition a
class SymAttrClefTransposition repr where  
  clefTransposition :: (AttrClefTransposition a) => Int -> repr (a ctx) -> repr (a ctx) 


  
-- Unfortunately this has to be an attibute of Pitch rather than ClefType
-- because of the pretty printing rules 
instance AttrClefTransposition Pitch

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

class AttrCmdLaissezVibrer a
class SymAttrCmdLaissezVibrer repr where
  laissezVibrer :: (AttrCmdLaissezVibrer a) => repr (a ctx) -> repr (a ctx)

instance AttrCmdLaissezVibrer Note
instance AttrCmdLaissezVibrer Chord

--------------------------------------------------------------------------------
-- *** Automatic beams (6.5.5)
-- noBeam is a note attribute

class AttrCmdNoBeam a
class SymAttrCmdNoBeam repr where
  noBeam :: (AttrCmdNoBeam a) => repr (a ctx) -> repr (a ctx)

instance AttrCmdNoBeam Note 

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


-- placement of an articulation, slur ...
data VerticalPlacement ctx
class SymVerticalPlacement repr where
  -- | Place a mark above the note with @^@.
  vabove   :: repr (VerticalPlacement ctx)
  -- | Place a mark below the note with @_@.
  vbelow   :: repr (VerticalPlacement ctx)
  vdefault :: repr (VerticalPlacement ctx)
  
--------------------------------------------------------------------------------
-- *** Fingering instructions (6.6.2)

class AttrFingering a
class SymAttrFingering repr where
  fingering       :: (AttrFingering a) => Int -> repr (a ctx) -> repr (a ctx) 

instance AttrFingering Note

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
class AttrCmdPedal a
class SymAttrCmdPedal repr where
  cmdPedal :: (AttrCmdPedal a) => String -> repr (a ctx) -> repr (a ctx)



instance AttrCmdPedal Note
instance AttrCmdPedal Chord

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
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

class AttrText a
class SymAttrText repr where
  attrtext        :: (AttrText a) => String -> repr (a ctx) -> repr (a ctx) 

data CmdFatText ctx 
class SymCmdFatText repr where
  fatText     :: repr (CmdFatText ctx) 
  
  
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


data CtxTabStaff ctx
class SymCtxTabStaff repr where
  tabStaff     :: repr (CtxTabStaff ctx)
  
instance ContextType CtxTabStaff




--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)
-- *** Multiple scores in a book (10.1.2)

data CmdScore ctx
class SymCmdScore repr where
  score :: repr (a subctx) -> repr (CmdScore CT_Book)

data CmdMarkup ctx
class SymCmdMarkup repr where
  markup  :: String -> repr (CmdMarkup ctx)

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
  block         :: repr (a subctx) -> repr (b superctx)

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
  
