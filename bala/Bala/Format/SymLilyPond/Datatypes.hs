{-# LANGUAGE EmptyDataDecls #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for a subset of LilyPond format
--
--------------------------------------------------------------------------------


-- Use the 'Finally Tagless...' approach
-- as it gives us open datatypes




module Bala.Format.SymLilyPond.Datatypes  where

import Bala.Format.Base.SymBase



-- | Contexts
data Ctx_Top
data Ctx_Book

data Ctx_Header  -- elements inside header block e.g. title dedication

data Ctx_Note 
data Ctx_NoteAttr

data Ctx_Element -- rest note skip etc
 

-- comments and versioning (2.12)


data CmdVersion ctx
class SymCmdVersion repr where 
  version :: String -> repr (CmdVersion Ctx_Top)
  
  

data LineComment ctx
class SymLineComment repr where
  lineComment :: String -> repr (LineComment ctx)

data BlockComment ctx
class SymBlockComment repr where
  blockComment :: String -> repr (BlockComment ctx)



-- pitches (6.1)

data PitchName = C | D | E | F | G | A | B 
  deriving (Eq,Show)

data Pitch ctx  
class SymPitch repr where
  pitch :: PitchName -> repr (Pitch ctx) 


  
class AttrOctaveSpec ctx
class SymAttrOctaveSpec repr where  
  raised      :: (AttrOctaveSpec a) => Int -> repr (a ctx) -> repr (a ctx)
  lowered     :: (AttrOctaveSpec a) => Int -> repr (a ctx) -> repr (a ctx)
  
instance AttrOctaveSpec Pitch

-- is this wise? It would make things a bit more polymorphic
-- instance AttrOctaveSpec Note   
              
data Note ctx
class SymNote repr where
  note :: repr (Pitch ctx) -> repr (Note Ctx_Element)

 
    
-- accidentals (6.1.2)  
class AttrAccidental ctx
class SymAttrAccidental repr where
  sharp       :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  flat        :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleSharp :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  doubleFlat  :: (AttrAccidental a) => repr (a ctx) -> repr (a ctx)
  
  
instance AttrAccidental Pitch  

-- cautionary accidentals (6.1.3)
data CautionaryAccidental ctx
class SymCautionaryAccidental repr where
  reminderAccidental    :: repr (CautionaryAccidental ctx)
  cautionaryAccidental  :: repr (CautionaryAccidental ctx)


-- Micro tones (6.1.4)    
class AttrMicroTone ctx
class SymAttrMicroTone repr where
  halfFlat  :: (AttrMicroTone a) => repr (a ctx) -> repr (a ctx)
  halfSharp :: (AttrMicroTone a) => repr (a ctx) -> repr (a ctx)
  
instance AttrMicroTone Pitch 


-- Relative octaves (6.1.6)

data CmdRelative ctx
class SymCmdReleative repr where
  relative :: repr (Pitch ctx) -> repr (b ctx) -> repr (CmdRelative Ctx_Note) 

  
 
-- rests (6.1.9)
data Rest ctx
class SymRest repr where
  rest :: repr (Rest ctx)
  
-- Skips (6.1.10)

-- "\skip"
data CmdSkip ctx
class SymCmdSkip repr where
  cmdSkip :: String -> repr (CmdSkip ctx)  
  
-- "s1", "s2", eyc  
data SkipDuration ctx
class SymSkipDuration repr where
  skipDuration :: repr (Duration ctx) -> repr (SkipDuration ctx)
  
-- durations (6.2)

data Duration ctx

class SymDuration repr where
  duration :: Int -> repr (Duration ctx)

class AttrDuration ctx
class SymAttrDuration repr where
  attrduration :: (AttrDuration a) => repr (Duration ctx) -> repr (a ctx) -> repr (a ctx)

class AttrDotted ctx
class SymAttrDotted repr where
  dotted :: (AttrDotted a) => Int -> repr (a ctx) -> repr (a ctx)

instance AttrDotted Duration


   
instance AttrDuration Rest
instance AttrDuration Note
instance AttrDuration Chord


class AttrCmdLongDuration ctx
class SymAttrCmdLongDuration repr where
  cmdLongDuration :: (AttrCmdLongDuration a) => String -> repr (a ctx) -> repr (a ctx)

instance AttrCmdLongDuration Rest
instance AttrCmdLongDuration Note
instance AttrCmdLongDuration Chord


  
-- tuplets (6.2.3)
data Times ctx
class SymTimes repr where
  times :: MeterFraction -> repr (a ctx) -> repr (Times ctx)


  
-- chords (6.3.1)
  
data Chord ctx
-- Use '[repr (Pitch ctx)]' as the list of pitches (not 'repr [repr (Pitch ctx)]')
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class SymChord repr where
  chord :: [repr (Pitch ctx)] -> repr (Chord ctx)


-- stems (6.3.2)
data CmdStem ctx
class SymCmdStem repr where
  cmdStem :: String -> repr (CmdStem ctx)


-- polyphony (6.3.3)
-- (Again this datatype should handle context)
data PolyCat ctx
class SymPolyCat repr where
  (\\) :: repr (a ctx) -> repr (a ctx) -> repr (PolyCat ctx)

-- Clef (6.4.1)
data CmdClef ctx
class SymCmdClef repr where
  clef :: repr (ClefType ctx) -> repr (CmdClef Ctx_Element)

 
data ClefType ctx
class SymClefType repr where
  cleftype :: String -> repr (ClefType ctx)


class AttrClefTransposition ctx
class SymAttrClefTransposition repr where  
  clefTransposition :: (AttrClefTransposition a) => Int -> repr (a ctx) -> repr (a ctx) 


  
-- Unfortunately this has to be an attibute of Pitch rather than ClefType
-- because of the pretty printing rules 
instance AttrClefTransposition Pitch

-- key signature (6.4.2)
data CmdKey ctx
class SymCmdKey repr where
  key :: repr (Pitch ctx) -> repr (CmdKeyType Ctx_Element) -> repr (CmdKey Ctx_Element)

data CmdKeyType ctx
class SymCmdKeyType repr where
  keyType :: String -> repr (CmdKeyType Ctx_Element)
  


-- Time signature (6.4.3)
data CmdTime ctx
class SymCmdTime repr where
  time :: MeterFraction -> repr (CmdTime Ctx_Element)


-- Bar lines (6.4.5)
data CmdBar ctx
class SymCmdBar repr where
  bar :: String -> repr (CmdBar ctx)
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

-- Unmetered music (6.4.6)

data CmdCadenza ctx
class SymCmdCadenza repr where
  cmdCadenza     :: String -> repr (CmdCadenza ctx)

    
  

-- Ties (6.5.1)
data Tie ctx
class SymTie repr where
  tie :: repr (Tie ctx)

data CmdTie ctx
class SymCmdTie repr where
  cmdTie :: String -> repr (CmdTie ctx)  




-- Slurs (6.5.2)
data Slur ctx
class SymSlur repr where
  openSlur  :: repr (Slur ctx)
  closeSlur :: repr (Slur ctx)
  
data CmdSlur ctx
class SymCmdSlur repr where
  cmdSlur :: String -> repr (CmdSlur ctx)  

-- Phrasing slurs (6.5.3)
-- { ATTRIBUTE OF NOTE ? }

data CmdPhrasingSlur ctx
class SymCmdPhrasingSlur repr where
  cmdPhrasingSlur             :: String -> repr (CmdPhrasingSlur ctx)


-- Laissez vibrer ties (6.5.4)
class AttrCmdLaissezVibrer ctx
class SymAttrCmdLaissezVibrer repr where
  laissezVibrer :: (AttrCmdLaissezVibrer a) => repr (a ctx) -> repr (a ctx)

instance AttrCmdLaissezVibrer Note
instance AttrCmdLaissezVibrer Chord

-- Automatic beams (6.5.5)
-- noBeam is a note attribute

class AttrCmdNoBeam ctx
class SymAttrCmdNoBeam repr where
  noBeam :: (AttrCmdNoBeam a) => repr (a ctx) -> repr (a ctx)

instance AttrCmdNoBeam Note 

-- Manual beams (6.5.6)
data Beam ctx
class SymBeam repr where
  openBeam  :: repr (Beam ctx)
  closeBeam :: repr (Beam ctx)
  
-- Grace notes (6.5.7)
data CmdGrace ctx
class SymCmdGrace repr where
  cmdGrace         :: String -> repr (CmdGrace ctx)


  

-- Articulations (6.6.1)
data CmdArticulation ctx
class SymCmdArticulation repr where
  cmdArticulation :: String -> repr (CmdArticulation ctx)


-- ~ Placement of an articulation, slur ...
data VerticalPlacement ctx
class SymVerticalPlacement repr where
  vabove   :: repr (VerticalPlacement ctx)
  vbelow   :: repr (VerticalPlacement ctx)
  vdefault :: repr (VerticalPlacement ctx)
  

-- Fingering instructions (6.6.2) 
class AttrFingering ctx
class SymAttrFingering repr where
  fingering :: (AttrFingering a) => Int -> repr (a ctx) -> repr (a ctx) 

instance AttrFingering Note

-- Dynamics (6.6.3)

-- so many dynamics that we parameterize the SymCmdDynamic signture with String
-- rather than have a seperate clause for each one
data CmdDynamic ctx
class SymCmdDynamic repr where
  cmdDynamic :: String -> repr (CmdDynamic ctx)
 

-- Breath marks (6.6.4)
-- { COULD JUST BE SINGLE CMD breathe ? } 
data CmdBreathe ctx
class SymCmdBreathe repr where
  cmdBreathe :: String -> repr (CmdBreathe ctx) 


-- Glissando (6.6.6)
-- { COULD JUST BE SINGLE CMD glissando ? } 
data CmdGlissando ctx
class SymCmdGlissando repr where
  cmdGlissando :: String -> repr (CmdGlissando ctx)
  


-- Arpeggio (6.6.7)
data CmdArpeggio ctx
class SymCmdArpeggio repr where
  cmdArpeggio          :: String -> repr (CmdArpeggio ctx)



-- Falls and doits (6.6.8)

data CmdBendAfter ctx 
class SymCmdBendAfter repr where
  bendAfter   :: repr (CmdBendAfter Ctx_Note) 




-- Metronome marks (8.2.2)

data CmdTempo ctx
class SymCmdTempo repr where
  tempo :: repr (Duration ctx) -> Int -> repr (CmdTempo ctx)


-- Creating contexts (9.2.2)
-- new is a binary command (type x music-expr)

data CmdNew ctx
class SymCmdNew repr where
  newContext  :: repr (ContextType ctx) -> repr (a ctx') -> repr (CmdNew ctx'')


data ContextType ctx
class SymContextType repr where
  contextType :: String -> repr (ContextType ctx)




-- Multiple scores in a book (10.1.2)

data CmdScore ctx
class SymCmdScore repr where
  score :: repr (a subctx) -> repr (CmdScore Ctx_Book)

data CmdMarkup ctx
class SymCmdMarkup repr where
  markup  :: String -> repr (CmdMarkup ctx)

data CmdBook ctx
class SymCmdBook repr where
  book :: repr (a subctx) -> repr (CmdBook Ctx_Top)
 



-- titles and headers (10.2)

data CmdHeader ctx
class SymCmdHeader repr where
  header :: repr (a Ctx_Header) -> repr (CmdHeader Ctx_Top)  
  

data Block ctx
class SymBlock repr where
  block :: repr (a subctx) -> repr (b superctx)
  
data EqnTitle ctx
class SymEqnTitle repr where
  title :: String -> repr (EqnTitle Ctx_Header)  
  
data EqnDedication ctx
class SymEqnDedication repr where
  dedication :: String -> repr (EqnDedication Ctx_Header) 
  
     
  