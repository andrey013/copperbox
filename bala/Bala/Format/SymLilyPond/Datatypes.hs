{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}


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

import Data.Ratio



-- | Contexts
data Ctx_Prologue
data Ctx_Header
data Ctx_Note 
data Ctx_NoteAttr

data Ctx_Element -- rest note skip etc


 
 
  
-- Nullary commands can share the same representation
data CmdZero ctx
class SymCmdZero repr where
  cmdZero :: String -> repr (CmdZero ctx)
  
 
data CmdOne ctx
class SymCmdOne repr where
  cmdOne :: String -> repr a -> repr (CmdOne ctx)
   
-- Equations (atributes or user named elements) can share the same 
-- representation  
data Equation ctx
class SymEquation repr where
  equation :: String -> repr a -> repr (Equation ctx)
  


-- This seems a bit of an unfortunated hack just so we can override
-- the pretty printer to output quoted strings  
data DoubleQuotes ctx
class SymDoubleQuotes repr where
  doubleQuotes :: String -> repr (DoubleQuotes ctx)


-- comments and versioning (2.12)


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


-- micro tones (6.1.4)    
data MicroTone ctx
class SymMicroTone repr where
  halfFlat  :: repr (MicroTone ctx) 
  halfSharp :: repr (MicroTone ctx)

  
 
-- rests (6.1.9)
data Rest ctx
class SymRest repr where
  rest :: repr (Rest ctx)
  
-- skips (6.1.10)
-- silent rest or nullary command
data Skip ctx
class SymSkip repr where
  skip :: repr (Skip ctx)
  
instance AttrDuration Skip  
  
  
  
-- durations (6.2)
class AttrDuration ctx
class SymDuration repr where
  dur :: (AttrDuration a) => Int -> repr (a ctx) -> repr (a ctx)
  dot :: (AttrDuration a) => repr (a ctx) -> repr (a ctx) 
     
instance AttrDuration Rest
instance AttrDuration Note
instance AttrDuration Chord
  
-- tuplets (6.2.3)
data Times ctx
class SymTimes repr where
  times :: Int -> Int -> repr (a ctx) -> repr (Times ctx)


  
-- chords (6.3.1)
  
data Chord ctx
-- Use '[repr (Pitch ctx)]' as the list of pitches (not 'repr [repr (Pitch ctx)]')
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class SymChord repr where
  chord :: [repr (Pitch ctx)] -> repr (Chord ctx)


-- stems (6.3.2)
-- nullary commands


-- polyphony (6.3.3)
-- (Again this datatype should handle context)
data PolyCat ctx
class SymPolyCat repr where
  (\\) :: repr (a ctx) -> repr (a ctx) -> repr (PolyCat ctx)

-- clef (6.4.1)
data CmdClef ctx
class SymCmdClef repr where
  cmdClef :: repr (Clef ctx) -> repr (CmdClef ctx)
  
data Clef ctx
class SymClef repr where
  clef :: String -> repr (Clef ctx)


-- key signature (6.4.2)
-- nullary commands 



-- time signature (6.4.3)
data CmdTime ctx
class SymCmdTime repr where
  cmdTime :: Rational -> repr (CmdTime Ctx_Note)


-- barlines (6.4.5)
data CmdBar ctx

class SymCmdBar repr where
  cmdBar :: String -> repr (CmdBar ctx)
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

-- unmetered music (6.4.6)
-- nullary commands
 
    
  

-- ties (6.5.1)
data Tie ctx
class SymTie repr where
  tie :: repr (Tie ctx)


-- slurs (6.5.2)
data Slur ctx
class SymSlur repr where
  openSlur  :: repr (Slur ctx)
  closeSlur :: repr (Slur ctx)

-- phrasing slurs (6.5.3)

data PhrasingSlur ctx
class SymPhrasingSlur repr where
  openPhrasingSlur  :: repr (PhrasingSlur ctx)
  closePhrasingSlur :: repr (PhrasingSlur ctx)



-- beams (6.5.6)
data Beam ctx
class SymBeam repr where
  openBeam  :: repr (Beam ctx)
  closeBeam :: repr (Beam ctx)
  

-- articulations (6.6.1)

-- ~ Placement of an articulation, slur ...
data VerticalPlacement ctx
class SymVerticalPlacement repr where
  vabove   :: repr (VerticalPlacement ctx)
  vbelow   :: repr (VerticalPlacement ctx)
  vdefault :: repr (VerticalPlacement ctx)
  

-- fingering instructions (6.6.2) 
class AttrFingering ctx
class SymAttrFingering repr where
  fingering :: (AttrFingering a) => Int -> repr (a ctx) -> repr (a ctx) 

instance AttrFingering Note

-- dynamics (6.6.3)
-- nullary commands 

data DynamicMark ctx
class SymDynamicMark repr where
  closeDynamic    :: repr (DynamicMark ctx)
  openCrescendo   :: repr (DynamicMark ctx)
  openDecrescendo :: repr (DynamicMark ctx)
  
-- breath marks (6.6.4)
-- nullary command


-- glissando (6.6.6)
-- nullary command


-- arpeggio (6.6.7)
-- nullary commands

-- falls and doits (6.6.8)
-- nullary command


-- titles and headers (10.2)

data HeaderBlock ctx
class SymHeaderBlock repr where
  headerBlock :: [repr (Equation Ctx_Header)] -> repr (HeaderBlock ctx)
  




  