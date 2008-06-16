


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.SyntaxElements
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A 'concrete' syntax built on the abstract syntax provided by the datatypes
--
--------------------------------------------------------------------------------

module Bala.Format.SymLilyPond.SyntaxElements where

import Bala.Format.Base.SymBase
import Bala.Format.SymLilyPond.Datatypes


-- comments and versioning (2.12)


-- pitches (6.1)

_c, _d, _e, _f, _g , _a, _b :: (SymPitch repr) => repr (Pitch ctx)
_c      = pitch C
_d      = pitch D
_e      = pitch E
_f      = pitch F
_g      = pitch G
_a      = pitch A
_b      = pitch B


-- pch'c

-- Relative octaves (6.1.6)


-- rests (6.1.9)
r1, r2, r4, r8, r16, r32, r64 
    :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r1      = rest # dur 1
r2      = rest # dur 2
r4      = rest # dur 4
r8      = rest # dur 8
r16     = rest # dur 16
r32     = rest # dur 32
r64     = rest # dur 64


-- skips (6.1.10)
skip :: (SymCmdSkip repr) => repr (CmdSkip ctx) 
skip = cmdSkip "skip"



s1, s2, s4, s8, s16, s32, s64 
  :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s1      = skipDuration $ duration 1
s2      = skipDuration $ duration 2
s4      = skipDuration $ duration 4
s8      = skipDuration $ duration 8
s16     = skipDuration $ duration 16
s32     = skipDuration $ duration 32
s64     = skipDuration $ duration 64


-- durations (6.2.1)
dur :: (SymAttrDuration repr, AttrDuration a, SymDuration repr) =>
       Int -> repr (a ctx) -> repr (a ctx)
dur i = attrduration $ duration i

dot :: (SymAttrDuration repr, AttrDuration a, SymDuration repr, SymAttrDotted repr) =>
       Int -> repr (a ctx) -> repr (a ctx)
dot i = attrduration $ dotted 1 $ duration i


dotdot :: (SymAttrDuration repr, AttrDuration a, SymDuration repr, SymAttrDotted repr) =>
       Int -> repr (a ctx) -> repr (a ctx)
dotdot i = attrduration $ dotted 2 $ duration i

-- dotted could store an Int of the dot count

longa :: (AttrCmdLongDuration a, SymAttrCmdLongDuration repr) => 
         repr (a Ctx_Element) -> repr (a Ctx_Element) 
longa   = cmdLongDuration "longa"  

breve :: (AttrCmdLongDuration a, SymAttrCmdLongDuration repr) => 
         repr (a Ctx_Element) -> repr (a Ctx_Element) 
breve   = cmdLongDuration "breve"

-- stems (6.3.2)

stemUp                  :: (SymCmdStem repr) => repr (CmdStem Ctx_Note)  
stemUp                  = cmdStem "stemUp"  

stemDown                :: (SymCmdStem repr) => repr (CmdStem Ctx_Note)  
stemDown                = cmdStem "stemDown"    

stemNeutral             :: (SymCmdStem repr) => repr (CmdStem Ctx_Note)  
stemNeutral             = cmdStem "stemNeutral"  


-- Clef (6.4.1)
treble, alto, tenor, bass, french, soprano, mezzosoprano, baritone, 
  varbaritone, subbass, percussion, tabClef
              :: SymClefType repr => repr (ClefType ctx)
treble        = cleftype "treble"
alto          = cleftype "alto"
tenor         = cleftype "temor"
bass          = cleftype "bass"
french        = cleftype "french"
soprano       = cleftype "soprano"
mezzosoprano  = cleftype "mezzosoprano"
baritone      = cleftype "baritone"
varbaritone   = cleftype "varbaritone"
subbass       = cleftype "subbass" 
percussion    = cleftype "percussion" 
tabClef       = cleftype "tabClef" 


clefUp8     :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefUp8     = clefTransposition 8

clefUp15    :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefUp15    = clefTransposition 15

clefDown8   :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefDown8   = clefTransposition (-8)

clefDown15  :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefDown15  = clefTransposition (-15)
  
-- key signature (6.4.2)


major, minor, ionian, locrian, aeolian, mixolydian, lydian, phrygian, dorian
      :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element)  
major                   = keyType "major"    
minor                   = keyType "minor"   
ionian                  = keyType "ionian"
locrian                 = keyType "locrian" 
aeolian                 = keyType "aeolian"
mixolydian              = keyType "mixolydian"
lydian                  = keyType "lydian"
phrygian                = keyType "phrygian"  
dorian                  = keyType "dorian" 

-- Time signature (6.4.3)

-- Bar lines (6.4.5)

-- Unmetered music (6.4.6)

cadenzaOn     :: (SymCmdCadenza repr) => repr (CmdCadenza ctx)
cadenzaOn     = cmdCadenza "cadenzaOn"

cadenzaOff    :: (SymCmdCadenza repr) => repr (CmdCadenza ctx)
cadenzaOff    = cmdCadenza "cadenzaOff"
  
-- Ties (6.5.1)

repeatTie               :: (SymCmdTie repr) => repr (CmdTie Ctx_Note)   
repeatTie               = cmdTie "repeatTie" 

tieUp, tieDown, tieNeutral, tieDotted, tieDashed, tieSolid
    :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
    
tieUp         = cmdTie "tieUp"
tieDown       = cmdTie "tieDown"
tieNeutral    = cmdTie "tieNeutral"
tieDotted     = cmdTie "tieDotted"
tieDashed     = cmdTie "tieDashed"
tieSolid      = cmdTie "tieSolid"


-- Slurs (6.5.2)

slurUp, slurDown, slurNeutral, slurDashed, slurDotted, slurSolid 
    :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurUp        = cmdSlur "slurUp"
slurDown      = cmdSlur "slurDown"
slurNeutral   = cmdSlur "slurNeutral"
slurDashed    = cmdSlur "slurDashed"
slurDotted    = cmdSlur "slurDotted"
slurSolid     = cmdSlur "slurSolid"

-- Phrasing slurs (6.5.3)
openPhrasingSlur, closePhrasingSlur 
    :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx)
openPhrasingSlur      = cmdPhrasingSlur "("
closePhrasingSlur     = cmdPhrasingSlur ")"

phrasingSlurUp, phrasingSlurDown, phrasingSlurNeutral
    :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx) 
phrasingSlurUp        = cmdPhrasingSlur "phrasingSlurUp"
phrasingSlurDown      = cmdPhrasingSlur "phrasingSlurDown"
phrasingSlurNeutral   = cmdPhrasingSlur "phrasingSlurNeutral"
  

-- Grace notes (6.5.7)

grace         :: SymCmdGrace repr => repr (CmdGrace ctx)
grace         = cmdGrace "grace"

acciaccatura  :: SymCmdGrace repr => repr (CmdGrace ctx)
acciaccatura  = cmdGrace "acciaccatura"

appoggiatura  :: SymCmdGrace repr => repr (CmdGrace ctx)
appoggiatura  = cmdGrace "appoggiatura" 
  
-- Articulations (6.6.1)
  
accent, marcato, staccatissimo, espressivo, 
    staccato, tenuto, portato, upbow,  downbow, 
    flageolet, thumb, lheel, rheel, ltoe, 
    rtoe, open, stopped, turn, reverseturn, 
    trill, prall, mordent, prallprall, prallmordent, 
    upprall, downprall, upmordent, downmordent,
    pralldown, prallup, lineprall, signumcongruentiae,
    shortfermata, fermata, longfermata, verylongfermata,
    segno, coda, varcoda
  :: (Attr repr, SymCmdArticulation repr) => repr (b Ctx_Element) -> repr (b Ctx_Element) 
    
             
accent                  = attr $ cmdArticulation "accent"  
marcato                 = attr $ cmdArticulation "marcato" 
staccatissimo           = attr $ cmdArticulation "staccatissimo" 
espressivo              = attr $ cmdArticulation "espressivo"  
staccato                = attr $ cmdArticulation "staccato" 
tenuto                  = attr $ cmdArticulation "tenuto" 
portato                 = attr $ cmdArticulation "portato"
upbow                   = attr $ cmdArticulation "upbow"
downbow                 = attr $ cmdArticulation "downbow" 
flageolet               = attr $ cmdArticulation "flageolet" 
thumb                   = attr $ cmdArticulation "thumb" 
lheel                   = attr $ cmdArticulation "lheel" 
rheel                   = attr $ cmdArticulation "rheel" 
ltoe                    = attr $ cmdArticulation "ltoe" 
rtoe                    = attr $ cmdArticulation "rtoe" 
open                    = attr $ cmdArticulation "open"
stopped                 = attr $ cmdArticulation "stopped"
turn                    = attr $ cmdArticulation "turn" 
reverseturn             = attr $ cmdArticulation "reverseturn"      
trill                   = attr $ cmdArticulation "trill"  
prall                   = attr $ cmdArticulation "prall"  
mordent                 = attr $ cmdArticulation "mordent"   
prallprall              = attr $ cmdArticulation "prallprall" 
prallmordent            = attr $ cmdArticulation "prallmordent"  
upprall                 = attr $ cmdArticulation "upprall"   
downprall               = attr $ cmdArticulation "downprall" 
upmordent               = attr $ cmdArticulation "upmordent"  
downmordent             = attr $ cmdArticulation "downmordent" 
pralldown               = attr $ cmdArticulation "pralldown"   
prallup                 = attr $ cmdArticulation "prallup"  
lineprall               = attr $ cmdArticulation "lineprall" 
signumcongruentiae      = attr $ cmdArticulation "signumcongruentiae"  
shortfermata            = attr $ cmdArticulation "shortfermata"  
fermata                 = attr $ cmdArticulation "fermata" 
longfermata             = attr $ cmdArticulation "longfermata" 
verylongfermata         = attr $ cmdArticulation "verylongfermata"    
segno                   = attr $ cmdArticulation "segno" 
coda                    = attr $ cmdArticulation "coda" 
varcoda                 = attr $ cmdArticulation "varcoda" 


-- dynamics (6.6.3)
-- nullary commands (use underscore suffix _ for commands)

ppppp_                  :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
ppppp_                  = cmdDynamic "ppppp" 

pppp_                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
pppp_                   = cmdDynamic "pppp" 

ppp_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
ppp_                    = cmdDynamic "ppp" 

pp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
pp_                     = cmdDynamic "pp" 

piano                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
piano                   = cmdDynamic "p" 

mp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
mp_                     = cmdDynamic "mp" 

mf_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
mf_                     = cmdDynamic "mf" 

forte                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
forte                   = cmdDynamic "f"

ff_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
ff_                     = cmdDynamic "ff"

fff_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
fff_                    = cmdDynamic "fff"

ffff_                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
ffff_                   = cmdDynamic "ffff"

fp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
fp_                     = cmdDynamic "fp"

sf_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sf_                     = cmdDynamic "sf"

sff_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sff_                    = cmdDynamic "sff"

sp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sp_                     = cmdDynamic "sp"

spp_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
spp_                    = cmdDynamic "spp"

sfz_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sfz_                    = cmdDynamic "sfz"

rfz_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
rfz_                    = cmdDynamic "rfz"

openCrescendo           :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
openCrescendo           = cmdDynamic "<"

openDecrescendo         :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
openDecrescendo         = cmdDynamic ">"
 
closeDynamic            :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
closeDynamic            = cmdDynamic "!"


cr_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
cr_                     = cmdDynamic "cr" 

decr_                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
decr_                   = cmdDynamic "decr"   
  
  
-- breath marks (6.6.4)

breathe       :: SymCmdBreathe repr => repr (CmdBreathe ctx) 
breathe       = cmdBreathe "breathe"

-- glissando (6.6.6)

glissando :: SymCmdGlissando repr => repr (CmdGlissando ctx)
glissando = cmdGlissando "glissando"  
  
-- arpeggio (6.6.7)
arpeggio, arpeggioUp, arpeggioDown, arpeggioNeutral, arpeggioBracket 
    :: SymCmdArpeggio repr => repr (CmdArpeggio ctx)
arpeggio          = cmdArpeggio "arpeggio"
arpeggioUp        = cmdArpeggio "arpeggioUp"
arpeggioDown      = cmdArpeggio "arpeggioDown"
arpeggioNeutral   = cmdArpeggio "arpeggioNeutral"  
arpeggioBracket   = cmdArpeggio "arpeggioBracket"
  
  
-- falls and doits (6.6.8)

-- Metronome marks (8.8.2)

-- Creating contexts (9.2.2)

staff :: (SymContextType repr) => repr (ContextType ctx)
staff = contextType "Staff"

voice :: (SymContextType repr) => repr (ContextType ctx)
voice = contextType "Voice"

tabStaff :: (SymContextType repr) => repr (ContextType ctx)
tabStaff = contextType "TabStaff"


-- Multiple scores in a book (10.1.2)



-- titles and headers (10.2)

