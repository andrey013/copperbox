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

-- Where commands are likely to clash with existing names they are prefixed 
-- with - c' or cmd


module Bala.Format.SymLilyPond.Datatypes  where

-- | Basic expression syntax

data Expr
class SymExpr repr where
  emp   :: repr Expr
  (##)  :: repr a -> repr b -> repr Expr

-- Nullary commands can share the same representation
data CmdZero
class SymCmdZero repr where
  cmdZero :: String -> repr CmdZero
  
  
class SymMaybe repr where
  just :: repr a -> repr (Maybe (repr a))
  nothing  :: repr (Maybe (repr a))

  


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
skip                    :: (SymCmdZero repr) => repr CmdZero  
skip                    = cmdZero "skip" 
  
  
-- durations (6.2)
data Duration  
class SymDuration repr where
  dur :: Int -> repr Duration
  dot :: Int -> repr Duration  
  
longa                   :: (SymCmdZero repr) => repr CmdZero  
longa                   = cmdZero "longa"  

breve                   :: (SymCmdZero repr) => repr CmdZero  
breve                   = cmdZero "breve"      
  
  
-- tuplets (6.2.3)
data Times
class SymTimes repr where
  times :: Int -> Int -> repr Expr -> repr Times


  
-- chords (6.3.1)
  
data Chord
-- Use '[repr Pitch]' as the list of pitches (not 'repr [repr Pitch]')
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class SymChord repr where
  chord :: [repr Pitch] -> repr (Maybe (repr Duration)) -> repr Chord


-- stems (6.3.2)

stemUp                  :: (SymCmdZero repr) => repr CmdZero  
stemUp                  = cmdZero "stemUp"  

stemDown                :: (SymCmdZero repr) => repr CmdZero  
stemDown                = cmdZero "stemDown"    

stemNeutral             :: (SymCmdZero repr) => repr CmdZero  
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
major                   :: (SymCmdZero repr) => repr CmdZero  
major                   = cmdZero "major"  

minor                   :: (SymCmdZero repr) => repr CmdZero  
minor                   = cmdZero "minor"  

ionian                  :: (SymCmdZero repr) => repr CmdZero  
ionian                  = cmdZero "ionian"
  
locrian                 :: (SymCmdZero repr) => repr CmdZero  
locrian                 = cmdZero "locrian" 
 
aeolian                 :: (SymCmdZero repr) => repr CmdZero  
aeolian                 = cmdZero "aeolian"
  
mixolydian              :: (SymCmdZero repr) => repr CmdZero  
mixolydian              = cmdZero "mixolydian"
  
lydian                  :: (SymCmdZero repr) => repr CmdZero  
lydian                  = cmdZero "lydian"
 
phrygian                :: (SymCmdZero repr) => repr CmdZero  
phrygian                = cmdZero "phrygian" 

dorian                  :: (SymCmdZero repr) => repr CmdZero  
dorian                  = cmdZero "dorian" 


-- time signature (6.4.3)
data CmdTime

class SymCmdTime repr where
  cmdTime :: Int -> Int -> repr CmdTime


-- barlines (6.4.5)
data CmdBar

class SymCmdBar repr where
  cmdBar :: String -> repr CmdBar
  
-- "|", "|:", "||", ":|", ".|", ".|.", ":|:", "|.", ":", "unbroken ||:",
-- "broken ||:"

-- unmetered music (6.4.6)
cadenzaOn               :: (SymCmdZero repr) => repr CmdZero  
cadenzaOn               = cmdZero "cadenzaOn" 

cadenzaOff              :: (SymCmdZero repr) => repr CmdZero  
cadenzaOff              = cmdZero "cadenzaOff" 
 
    
  

-- ties (6.5.1)
data Tie
class SymTie repr where
  tie :: repr Tie

repeatTie               :: (SymCmdZero repr) => repr CmdZero  
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
  
  
accent                  :: (SymCmdZero repr) => repr CmdZero  
accent                  = cmdZero "accent"  

marcato                 :: (SymCmdZero repr) => repr CmdZero  
marcato                 = cmdZero "marcato" 

staccatissimo           :: (SymCmdZero repr) => repr CmdZero  
staccatissimo           = cmdZero "staccatissimo" 

espressivo              :: (SymCmdZero repr) => repr CmdZero  
espressivo              = cmdZero "espressivo"  

staccato                :: (SymCmdZero repr) => repr CmdZero  
staccato                = cmdZero "staccato" 
 
tenuto                  :: (SymCmdZero repr) => repr CmdZero  
tenuto                  = cmdZero "tenuto" 

portato                 :: (SymCmdZero repr) => repr CmdZero  
portato                 = cmdZero "portato"
  
upbow                   :: (SymCmdZero repr) => repr CmdZero  
upbow                   = cmdZero "upbow"
 
downbow                 :: (SymCmdZero repr) => repr CmdZero  
downbow                 = cmdZero "downbow" 
      
flageolet               :: (SymCmdZero repr) => repr CmdZero  
flageolet               = cmdZero "flageolet" 

thumb                   :: (SymCmdZero repr) => repr CmdZero  
thumb                   = cmdZero "thumb" 
  
lheel                   :: (SymCmdZero repr) => repr CmdZero  
lheel                   = cmdZero "lheel" 

rheel                   :: (SymCmdZero repr) => repr CmdZero  
rheel                   = cmdZero "rheel" 

ltoe                    :: (SymCmdZero repr) => repr CmdZero  
ltoe                    = cmdZero "ltoe" 
      
rtoe                    :: (SymCmdZero repr) => repr CmdZero  
rtoe                    = cmdZero "rtoe" 

open                    :: (SymCmdZero repr) => repr CmdZero  
open                    = cmdZero "open"
 
stopped                 :: (SymCmdZero repr) => repr CmdZero  
stopped                 = cmdZero "stopped"
 
turn                    :: (SymCmdZero repr) => repr CmdZero  
turn                    = cmdZero "turn" 

reverseturn             :: (SymCmdZero repr) => repr CmdZero  
reverseturn             = cmdZero "reverseturn"      

trill                   :: (SymCmdZero repr) => repr CmdZero  
trill                   = cmdZero "trill"  

prall                   :: (SymCmdZero repr) => repr CmdZero  
prall                   = cmdZero "prall" 
 
mordent                 :: (SymCmdZero repr) => repr CmdZero  
mordent                 = cmdZero "mordent"  
 
prallprall              :: (SymCmdZero repr) => repr CmdZero  
prallprall              = cmdZero "prallprall"
  
prallmordent            :: (SymCmdZero repr) => repr CmdZero  
prallmordent            = cmdZero "prallmordent"  

upprall                 :: (SymCmdZero repr) => repr CmdZero  
upprall                 = cmdZero "upprall"  

downprall               :: (SymCmdZero repr) => repr CmdZero  
downprall               = cmdZero "downprall"
 
upmordent               :: (SymCmdZero repr) => repr CmdZero  
upmordent               = cmdZero "upmordent"
 
downmordent             :: (SymCmdZero repr) => repr CmdZero  
downmordent             = cmdZero "downmordent" 

  
pralldown               :: (SymCmdZero repr) => repr CmdZero  
pralldown               = cmdZero "pralldown" 

prallup                 :: (SymCmdZero repr) => repr CmdZero  
prallup                 = cmdZero "prallup" 

lineprall               :: (SymCmdZero repr) => repr CmdZero  
lineprall               = cmdZero "lineprall" 

signumcongruentiae      :: (SymCmdZero repr) => repr CmdZero  
signumcongruentiae      = cmdZero "signumcongruentiae" 

shortfermata            :: (SymCmdZero repr) => repr CmdZero  
shortfermata            = cmdZero "shortfermata"  

fermata                 :: (SymCmdZero repr) => repr CmdZero  
fermata                 = cmdZero "fermata" 

longfermata             :: (SymCmdZero repr) => repr CmdZero  
longfermata             = cmdZero "longfermata" 

verylongfermata         :: (SymCmdZero repr) => repr CmdZero  
verylongfermata         = cmdZero "verylongfermata" 

segno                   :: (SymCmdZero repr) => repr CmdZero  
segno                   = cmdZero "segno" 

coda                    :: (SymCmdZero repr) => repr CmdZero  
coda                    = cmdZero "coda" 

varcoda                 :: (SymCmdZero repr) => repr CmdZero  
varcoda                 = cmdZero "varcoda" 



-- fingering instructions (6.6.2) [these seem to imply that 'note' is a context]
data FingeringInst 
class SymFingeringInst repr where
  fingeringInst :: Int -> repr FingeringInst 



-- dynamics (6.6.3)
-- nullary commands (use the prefix c' for commands)

c'ppppp                 :: (SymCmdZero repr) => repr CmdZero  
c'ppppp                 = cmdZero "ppppp" 

c'pppp                  :: (SymCmdZero repr) => repr CmdZero  
c'pppp                  = cmdZero "pppp" 

c'ppp                   :: (SymCmdZero repr) => repr CmdZero  
c'ppp                   = cmdZero "ppp" 

c'pp                    :: (SymCmdZero repr) => repr CmdZero  
c'pp                    = cmdZero "pp" 

c'mp                    :: (SymCmdZero repr) => repr CmdZero  
c'mp                    = cmdZero "mp" 

c'mf                    :: (SymCmdZero repr) => repr CmdZero  
c'mf                    = cmdZero "mf" 

c'f                     :: (SymCmdZero repr) => repr CmdZero 
c'f                     = cmdZero "f"

c'ff                    :: (SymCmdZero repr) => repr CmdZero 
c'ff                    = cmdZero "ff"

c'fff                  :: (SymCmdZero repr) => repr CmdZero 
c'fff                   = cmdZero "fff"

c'ffff                 :: (SymCmdZero repr) => repr CmdZero 
c'ffff                  = cmdZero "ffff"

c'fp                    :: (SymCmdZero repr) => repr CmdZero 
c'fp                    = cmdZero "fp"

c'sf                    :: (SymCmdZero repr) => repr CmdZero 
c'sf                    = cmdZero "sf"

c'sff                   :: (SymCmdZero repr) => repr CmdZero 
c'sff                   = cmdZero "sff"

c'sp                    :: (SymCmdZero repr) => repr CmdZero 
c'sp                    = cmdZero "sp"

c'spp                   :: (SymCmdZero repr) => repr CmdZero 
c'spp                   = cmdZero "spp"

c'sfz                   :: (SymCmdZero repr) => repr CmdZero 
c'sfz                   = cmdZero "sfz"

c'rfz                   :: (SymCmdZero repr) => repr CmdZero 
c'rfz                   = cmdZero "rfz"

data DynamicMark
class SymDynamicMark repr where
  closeDynamic    :: repr DynamicMark
  openCrescendo   :: repr DynamicMark
  openDecrescendo :: repr DynamicMark
  
-- breath marks (6.6.4)

breathe                 :: (SymCmdZero repr) => repr CmdZero 
breathe                 = cmdZero "breathe"

-- glissando (6.6.6)
glissando               :: (SymCmdZero repr) => repr CmdZero 
glissando               = cmdZero "glissando"


-- arpeggio (6.6.7) -- indicates 'chord context'

arpeggio                :: (SymCmdZero repr) => repr CmdZero 
arpeggio                = cmdZero "arpeggio"

arpeggioBracket         :: (SymCmdZero repr) => repr CmdZero 
arpeggioBracket         = cmdZero "arpeggioBracket"

arpeggioUp              :: (SymCmdZero repr) => repr CmdZero 
arpeggioUp              = cmdZero "arpeggioUp"

arpeggioDown            :: (SymCmdZero repr) => repr CmdZero 
arpeggioDown            = cmdZero "arpeggioDown"

-- falls and doits (6.6.8)

bendAfter               :: (SymCmdZero repr) => repr CmdZero 
bendAfter               = cmdZero "bendAfter"



  