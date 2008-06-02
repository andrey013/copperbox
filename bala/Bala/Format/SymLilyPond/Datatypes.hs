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

-- | Basic expression syntax
class SymE repr where
  cmd   :: String -> repr a -> repr b
  emp   :: repr ()
  (##)  :: repr a -> repr b -> repr c


class SymMaybe repr where
  just :: repr a -> repr (Maybe (repr a))
  nothing  :: repr (Maybe (repr a))

  
  
  
data Duration  
class SymDuration repr where
  dur :: Int -> repr Duration
  dot :: Int -> repr Duration
  

data Rest
class SymRest repr where
  rest :: repr Duration -> repr Rest


data Pitch = C | D | E | F | G | A | B 
  deriving (Eq,Show)

class SymPitch repr where
  pitch :: Pitch -> repr Pitch 

-- alternatively we could have all theses as members of class pitch...
c_, d_, e_, f_, g_ , a_, b_ :: (SymPitch repr) => repr Pitch
c_  = pitch C
d_  = pitch D
e_  = pitch E
f_  = pitch F
g_  = pitch G
a_  = pitch A
b_  = pitch B


data Note
class SymNote repr where
  note :: repr Pitch -> repr (Maybe (repr Duration)) -> repr Note
  
 
data Accidental
class SymAccidental repr where
  sharp       :: repr Accidental
  flat        :: repr Accidental
  doubleSharp :: repr Accidental
  doubleFlat  :: repr Accidental


  
data OctaveSpec
class SymOctaveSpec repr where  
  raised  :: Int -> repr OctaveSpec
  lowered :: Int -> repr OctaveSpec
  
   
data Articulation 
class SymArticulation repr where
  articulation :: repr VerticalPlacement -> Char -> repr Articulation
  fingering    :: repr VerticalPlacement -> Int  -> repr Articulation
    

-- ~ Placement of an articulation, slur ...
data VerticalPlacement 
class SymVerticalPlacement repr where
  vAbove   :: repr VerticalPlacement
  vBelow   :: repr VerticalPlacement
  vDefault :: repr VerticalPlacement

  
    
data MicroTone
class SymMicroTone repr where
  halfFlat  :: repr MicroTone 
  halfSharp :: repr MicroTone
  
data Chord
-- Use '[repr Pitch]' as the list of pitches (not 'repr [repr Pitch]')
-- For lists it is handy to keep the special constructor syntax and pattern 
-- matching (whereas with the Maybe type there is no special syntax so 
-- the final-tagless version is preferred).
class SymChord repr where
  chord :: [repr Pitch] -> repr (Maybe (repr Duration)) -> repr Chord



-- are Commands actually better handled outside SymE ?
data CmdTime

class SymCmdTime repr where
  cmdTime :: Int -> Int -> repr CmdTime

-- Nullary commands can share the same representation
data CmdZero
class SymCmdZero repr where
  cmdZero :: String -> repr CmdZero


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
dorian                   = cmdZero "dorian" 


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

  





