{-# LANGUAGE MultiParamTypeClasses #-}


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
    
version :: (SymCmdOne repr, SymDoubleQuotes repr) => 
    String -> repr (CmdOne Ctx_Prologue)  
version s               = cmdOne "version" (doubleQuotes s) 


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

-- rests (6.1.9)
r1, r2, r4, r8, r16, r32, r64 :: (SymRest repr, SymAttrDuration repr) => repr (Rest ctx)
r1      = rest # dur 1
r2      = rest # dur 2
r4      = rest # dur 4
r8      = rest # dur 8
r16     = rest # dur 16
r32     = rest # dur 32
r64     = rest # dur 64


-- skips (6.1.10)
skip_                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
skip_                   = cmdZero "skip" 

s1, s2, s4, s8, s16, s32, s64 :: (SymSkip repr, SymAttrDuration repr) => repr (Skip ctx)
s1      = skip # dur 1
s2      = skip # dur 2
s4      = skip # dur 4
s8      = skip # dur 8
s16     = skip # dur 16
s32     = skip # dur 32
s64     = skip # dur 64


-- durations (6.2)
longa :: (Attr repr, SymCmdZero repr) => 
    repr (b Ctx_Element) -> repr (b Ctx_Element) 
longa                   = attr $ cmdZero "longa"  

breve :: (Attr repr, SymCmdZero repr) => 
    repr (b Ctx_Element) -> repr (b Ctx_Element) 
breve                   = attr $ cmdZero "breve"

-- stems (6.3.2)

stemUp                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stemUp                  = cmdZero "stemUp"  

stemDown                :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stemDown                = cmdZero "stemDown"    

stemNeutral             :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
stemNeutral             = cmdZero "stemNeutral"  


-- clef (6.4.1)
treble, alto, tenor, bass, french, soprano, mezzosoprano, baritone, 
  varbaritone, subbass, percussion, tabClef
              :: SymClef repr => repr (Clef ctx)
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

-- unmetered music (6.4.6)
cadenzaOn               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
cadenzaOn               = cmdZero "cadenzaOn" 

cadenzaOff              :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
cadenzaOff              = cmdZero "cadenzaOff" 

-- ties (6.5.1)

repeatTie               :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)   
repeatTie               = cmdZero "repeatTie" 


-- articulations (6.6.1)
  
accent, marcato, staccatissimo, espressivo, 
    staccato, tenuto, portato, upbow,  downbow, 
    flageolet, thumb, lheel, rheel, ltoe, 
    rtoe, open, stopped, turn, reverseturn, 
    trill, prall, mordent, prallprall, prallmordent, 
    upprall, downprall, upmordent, downmordent,
    pralldown, prallup, lineprall, signumcongruentiae,
    shortfermata, fermata, longfermata, verylongfermata,
    segno, coda, varcoda
  :: (Attr repr, SymCmdZero repr) => repr (b Ctx_Element) -> repr (b Ctx_Element) 
    
             
accent                  = attr $ cmdZero "accent"  
marcato                 = attr $ cmdZero "marcato" 
staccatissimo           = attr $ cmdZero "staccatissimo" 
espressivo              = attr $ cmdZero "espressivo"  
staccato                = attr $ cmdZero "staccato" 
tenuto                  = attr $ cmdZero "tenuto" 
portato                 = attr $ cmdZero "portato"
upbow                   = attr $ cmdZero "upbow"
downbow                 = attr $ cmdZero "downbow" 
flageolet               = attr $ cmdZero "flageolet" 
thumb                   = attr $ cmdZero "thumb" 
lheel                   = attr $ cmdZero "lheel" 
rheel                   = attr $ cmdZero "rheel" 
ltoe                    = attr $ cmdZero "ltoe" 
rtoe                    = attr $ cmdZero "rtoe" 
open                    = attr $ cmdZero "open"
stopped                 = attr $ cmdZero "stopped"
turn                    = attr $ cmdZero "turn" 
reverseturn             = attr $ cmdZero "reverseturn"      
trill                   = attr $ cmdZero "trill"  
prall                   = attr $ cmdZero "prall"  
mordent                 = attr $ cmdZero "mordent"   
prallprall              = attr $ cmdZero "prallprall" 
prallmordent            = attr $ cmdZero "prallmordent"  
upprall                 = attr $ cmdZero "upprall"   
downprall               = attr $ cmdZero "downprall" 
upmordent               = attr $ cmdZero "upmordent"  
downmordent             = attr $ cmdZero "downmordent" 
pralldown               = attr $ cmdZero "pralldown"   
prallup                 = attr $ cmdZero "prallup"  
lineprall               = attr $ cmdZero "lineprall" 
signumcongruentiae      = attr $ cmdZero "signumcongruentiae"  
shortfermata            = attr $ cmdZero "shortfermata"  
fermata                 = attr $ cmdZero "fermata" 
longfermata             = attr $ cmdZero "longfermata" 
verylongfermata         = attr $ cmdZero "verylongfermata"    
segno                   = attr $ cmdZero "segno" 
coda                    = attr $ cmdZero "coda" 
varcoda                 = attr $ cmdZero "varcoda" 


-- dynamics (6.6.3)
-- nullary commands (use underscore suffix _ for commands)

ppppp_                  :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
ppppp_                  = cmdZero "ppppp" 

pppp_                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
pppp_                   = cmdZero "pppp" 

ppp_                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
ppp_                    = cmdZero "ppp" 

pp_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
pp_                     = cmdZero "pp" 

piano                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
piano                   = cmdZero "p" 

mp_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
mp_                     = cmdZero "mp" 

mf_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
mf_                     = cmdZero "mf" 


forte                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
forte                   = cmdZero "f"

ff_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
ff_                     = cmdZero "ff"

fff_                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
fff_                    = cmdZero "fff"

ffff_                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
ffff_                   = cmdZero "ffff"

fp_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note) 
fp_                     = cmdZero "fp"

sf_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
sf_                     = cmdZero "sf"

sff_                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
sff_                    = cmdZero "sff"

sp_                     :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
sp_                     = cmdZero "sp"

spp_                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
spp_                    = cmdZero "spp"

sfz_                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
sfz_                    = cmdZero "sfz"

rfz_                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
rfz_                    = cmdZero "rfz"

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
