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

-- Where commands are likely to clash with existing names they are prefixed 
-- with - c' or cmd


module Bala.Format.SymLilyPond.Datatypes  where


-- | Contexts
data Ctx_Prologue
data Ctx_Header
data Ctx_Note 
data Ctx_NoteAttr



data Concatenation ctx

infixl 7 +++
class SymConcatenation ctx repr where
  (+++)  :: repr (a ctx) -> repr (b ctx) -> repr (Concatenation ctx)
  


class SymString repr where
  withString :: String -> repr String
  
class SymInt repr where
  withInt :: Int -> repr Int  


class SymMaybe repr where
  just :: repr a -> repr (Maybe (repr a))
  nothing  :: repr (Maybe (repr a))
  
  
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
data DoubleQuotes
class SymDoubleQuotes repr where
  doubleQuotes :: String -> repr DoubleQuotes


-- comments and versioning (2.12)

-- ctx? Comments can appear 'anywhere' 

data LineComment 
class SymLineComment repr where
  lineComment :: String -> repr LineComment

data BlockComment
class SymBlockComment repr where
  blockComment :: String -> repr BlockComment



    
version :: (SymCmdOne repr, SymDoubleQuotes repr) => 
    String -> repr (CmdOne Ctx_Prologue)  
version s               = cmdOne "version" (doubleQuotes s) 

    
    

   

-- pitches (6.1)

data PitchName = C | D | E | F | G | A | B 
  deriving (Eq,Show)

class SymPitchName repr where
  pitchName :: PitchName -> repr PitchName 

-- alternatively we could have all theses as members of class pitch...
c_, d_, e_, f_, g_ , a_, b_ :: (SymPitchName repr) => repr PitchName
c_  = pitchName C
d_  = pitchName D
e_  = pitchName E
f_  = pitchName F
g_  = pitchName G
a_  = pitchName A
b_  = pitchName B

  
data OctaveSpec
class SymOctaveSpec repr where  
  raised  :: Int -> repr OctaveSpec
  lowered :: Int -> repr OctaveSpec
  

-- rather than the implementation below,
-- can accidentals and octavespec be attributes of a note?
  
data Pitch  
class SymPitch repr where
  pitch :: repr PitchName -> repr (Maybe (repr Accidental)) 
              -> repr (Maybe (repr OctaveSpec)) -> repr Pitch 
              
data Note
class SymNote repr where
  note :: repr Pitch -> repr (Maybe (repr Duration)) -> repr Note

 
    
-- accidentals (6.1.2)  
data Accidental
class SymAccidental repr where
  sharp       :: repr Accidental
  flat        :: repr Accidental
  doubleSharp :: repr Accidental
  doubleFlat  :: repr Accidental

-- cautionary accidentals (6.1.3)
data CautionaryAccidental
class SymCautionaryAccidental repr where
  reminderAccidental    :: repr CautionaryAccidental
  cautionaryAccidental  :: repr CautionaryAccidental


-- micro tones (6.1.4)    
data MicroTone
class SymMicroTone repr where
  halfFlat  :: repr MicroTone 
  halfSharp :: repr MicroTone

  
 
-- rests (6.1.9)
data Rest
class SymRest repr where
  rest :: repr Duration -> repr Rest
  
-- skips (6.1.10)
skip                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
skip                    = cmdZero "skip" 
  
  
-- durations (6.2)
data Duration  
class SymDuration repr where
  dur :: Int -> repr Duration
  dot :: Int -> repr Duration  
  
longa                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
longa                   = cmdZero "longa"  

breve                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
breve                   = cmdZero "breve"      
  
  
-- tuplets (6.2.3)
data Times
class SymTimes repr where
  times :: Int -> Int -> repr a -> repr Times


  
-- chords (6.3.1)
  
data Chord
-- Use '[repr Pitch]' as the list of pitches (not 'repr [repr Pitch]')
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class SymChord repr where
  chord :: [repr Pitch] -> repr (Maybe (repr Duration)) -> repr Chord


-- stems (6.3.2)

stemUp                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stemUp                  = cmdZero "stemUp"  

stemDown                :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stemDown                = cmdZero "stemDown"    

stemNeutral             :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stemNeutral             = cmdZero "stemNeutral"  

-- polyphony (6.3.3)
-- (Again this datatype should handle context)
data PolyCat
class SymPolyCat repr where
  (\\) :: repr a -> repr a -> repr PolyCat

-- clef (6.4.1)
data CmdClef 
class SymCmdClef repr where
  cmdClef :: repr Clef -> repr CmdClef
  
data Clef 

class SymClef repr where
  clef :: String -> repr Clef

treble, alto, tenor, bass, french, soprano, mezzosoprano, baritone, 
  varbaritone, subbass, percussion, tabClef
              :: SymClef repr => repr Clef
treble        = clef "treble"
alto          = clef "alto"
tenor         = clef "temor"
bass          = clef "bass"
french        = clef "french"
soprano       = clef "soprano"
mezzosoprano  = clef "mezzosoprano"
baritone      = clef "baritone"
varbaritone   = clef "varbaritone"
subbass       = clef "subbass" 
percussion    = clef "percussion" 
tabClef       = clef "tabClef" 

    


-- key signature (6.4.2)
major                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
major                   = cmdZero "major"  

minor                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
minor                   = cmdZero "minor"  

ionian                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
ionian                  = cmdZero "ionian"
  
locrian                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
locrian                 = cmdZero "locrian" 
 
aeolian                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
aeolian                 = cmdZero "aeolian"
  
mixolydian              :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
mixolydian              = cmdZero "mixolydian"
  
lydian                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
lydian                  = cmdZero "lydian"
 
phrygian                :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
phrygian                = cmdZero "phrygian" 

dorian                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
dorian                  = cmdZero "dorian" 


-- time signature (6.4.3)
data CmdTime

class SymCmdTime repr where
  cmdTime :: Int -> Int -> repr (CmdZero Ctx_Note)


-- barlines (6.4.5)
data CmdBar

class SymCmdBar repr where
  cmdBar :: String -> repr CmdBar
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

-- unmetered music (6.4.6)
cadenzaOn               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
cadenzaOn               = cmdZero "cadenzaOn" 

cadenzaOff              :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
cadenzaOff              = cmdZero "cadenzaOff" 
 
    
  

-- ties (6.5.1)
data Tie
class SymTie repr where
  tie :: repr Tie

repeatTie               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)   
repeatTie               = cmdZero "repeatTie" 

-- slurs (6.5.2)
data Slur
class SymSlur repr where
  openSlur  :: repr Slur
  closeSlur :: repr Slur

-- phrasing slurs (6.5.3)

data PhrasingSlur
class SymPhrasingSlur repr where
  openPhrasingSlur  :: repr PhrasingSlur
  closePhrasingSlur :: repr PhrasingSlur



-- beams (6.5.6)
data Beam
class SymBeam repr where
  openBeam  :: repr Beam
  closeBeam :: repr Beam
  

-- articulations (6.6.1)

-- ~ Placement of an articulation, slur ...
data VerticalPlacement 
class SymVerticalPlacement repr where
  vAbove   :: repr VerticalPlacement
  vBelow   :: repr VerticalPlacement
  vDefault :: repr VerticalPlacement
  
  
accent                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
accent                  = cmdZero "accent"  

marcato                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
marcato                 = cmdZero "marcato" 

staccatissimo           :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
staccatissimo           = cmdZero "staccatissimo" 

espressivo              :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
espressivo              = cmdZero "espressivo"  

staccato                :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
staccato                = cmdZero "staccato" 
 
tenuto                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
tenuto                  = cmdZero "tenuto" 

portato                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
portato                 = cmdZero "portato"
  
upbow                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
upbow                   = cmdZero "upbow"
 
downbow                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
downbow                 = cmdZero "downbow" 
      
flageolet               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
flageolet               = cmdZero "flageolet" 

thumb                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
thumb                   = cmdZero "thumb" 
  
lheel                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
lheel                   = cmdZero "lheel" 

rheel                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
rheel                   = cmdZero "rheel" 

ltoe                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
ltoe                    = cmdZero "ltoe" 
      
rtoe                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
rtoe                    = cmdZero "rtoe" 

open                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
open                    = cmdZero "open"
 
stopped                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stopped                 = cmdZero "stopped"
 
turn                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
turn                    = cmdZero "turn" 

reverseturn             :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
reverseturn             = cmdZero "reverseturn"      

trill                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
trill                   = cmdZero "trill"  

prall                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
prall                   = cmdZero "prall" 
 
mordent                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
mordent                 = cmdZero "mordent"  
 
prallprall              :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
prallprall              = cmdZero "prallprall"
  
prallmordent            :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
prallmordent            = cmdZero "prallmordent"  

upprall                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
upprall                 = cmdZero "upprall"  

downprall               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
downprall               = cmdZero "downprall"
 
upmordent               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
upmordent               = cmdZero "upmordent"
 
downmordent             :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
downmordent             = cmdZero "downmordent" 

  
pralldown               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
pralldown               = cmdZero "pralldown" 

prallup                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
prallup                 = cmdZero "prallup" 

lineprall               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
lineprall               = cmdZero "lineprall" 

signumcongruentiae      :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
signumcongruentiae      = cmdZero "signumcongruentiae" 

shortfermata            :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
shortfermata            = cmdZero "shortfermata"  

fermata                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
fermata                 = cmdZero "fermata" 

longfermata             :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
longfermata             = cmdZero "longfermata" 

verylongfermata         :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
verylongfermata         = cmdZero "verylongfermata" 

segno                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
segno                   = cmdZero "segno" 

coda                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
coda                    = cmdZero "coda" 

varcoda                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
varcoda                 = cmdZero "varcoda" 



-- fingering instructions (6.6.2) [these seem to imply that 'note' is a context]
data FingeringInst ctx
class SymFingeringInst repr where
  fingeringInst :: Int -> repr (FingeringInst Ctx_Note) 



-- dynamics (6.6.3)
-- nullary commands (use the prefix c' for commands)

c'ppppp                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'ppppp                 = cmdZero "ppppp" 

c'pppp                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'pppp                  = cmdZero "pppp" 

c'ppp                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'ppp                   = cmdZero "ppp" 

c'pp                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'pp                    = cmdZero "pp" 

c'mp                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'mp                    = cmdZero "mp" 

c'mf                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'mf                    = cmdZero "mf" 

c'f                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
c'f                     = cmdZero "f"

c'ff                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
c'ff                    = cmdZero "ff"

c'fff                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
c'fff                   = cmdZero "fff"

c'ffff                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
c'ffff                  = cmdZero "ffff"

c'fp                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
c'fp                    = cmdZero "fp"

c'sf                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'sf                    = cmdZero "sf"

c'sff                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'sff                   = cmdZero "sff"

c'sp                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'sp                    = cmdZero "sp"

c'spp                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'spp                   = cmdZero "spp"

c'sfz                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'sfz                   = cmdZero "sfz"

c'rfz                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
c'rfz                   = cmdZero "rfz"

data DynamicMark
class SymDynamicMark repr where
  closeDynamic    :: repr DynamicMark
  openCrescendo   :: repr DynamicMark
  openDecrescendo :: repr DynamicMark
  
-- breath marks (6.6.4)

breathe                 :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
breathe                 = cmdZero "breathe"

-- glissando (6.6.6)
glissando               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
glissando               = cmdZero "glissando"


-- arpeggio (6.6.7) -- indicates 'chord context'

arpeggio                :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
arpeggio                = cmdZero "arpeggio"

arpeggioBracket         :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
arpeggioBracket         = cmdZero "arpeggioBracket"

arpeggioUp              :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
arpeggioUp              = cmdZero "arpeggioUp"

arpeggioDown            :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
arpeggioDown            = cmdZero "arpeggioDown"

-- falls and doits (6.6.8)

bendAfter               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
bendAfter               = cmdZero "bendAfter"


-- titles and headers (10.2)



data HeaderBlock
class SymHeaderBlock repr where
  headerBlock :: [repr (Equation Ctx_Header)] -> repr HeaderBlock
  
-- header :: (SymCmdOne repr, SymHeaderBlock repr) => 
--     repr HeaderBlock -> repr (CmdOne Ctx_Prologue)

header :: (SymCmdOne repr, SymHeaderBlock repr) => 
          [repr (Equation Ctx_Header)] -> repr (CmdOne Ctx_Prologue)  
header xs               = cmdOne "header" (headerBlock xs)


-- These are simplifications, header attributes can have e.g \markup cmds
dedication :: (SymEquation repr, SymDoubleQuotes repr) => 
    String -> repr (Equation Ctx_Header)
dedication s = equation "dedication" (doubleQuotes s)



title :: (SymEquation repr, SymDoubleQuotes repr) => 
    String -> repr (Equation Ctx_Header)   
title s = equation "title" (doubleQuotes s)

  