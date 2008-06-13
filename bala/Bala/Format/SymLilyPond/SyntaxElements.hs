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

import Bala.Format.SymLilyPond.Datatypes


-- comments and versioning (2.12)
    
version :: (SymCmdOne repr, SymDoubleQuotes repr) => 
    String -> repr (CmdOne Ctx_Prologue)  
version s               = cmdOne "version" (doubleQuotes s) 


-- pitches (6.1)

c_, d_, e_, f_, g_ , a_, b_ :: (SymPitchName repr) => repr PitchName
c_  = pitchName C
d_  = pitchName D
e_  = pitchName E
f_  = pitchName F
g_  = pitchName G
a_  = pitchName A
b_  = pitchName B


-- skips (6.1.10)
skip                    :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
skip                    = cmdZero "skip" 


-- durations (6.2)
longa                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
longa                   = cmdZero "longa"  

breve                   :: (SymCmdZero repr) => repr (CmdZero Ctx_Note)  
breve                   = cmdZero "breve"

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
