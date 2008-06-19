
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


-- * Pitches (6.1)

_c, _d, _e, _f, _g , _a, _b :: (SymPitch repr) => repr (Pitch ctx)
_c      = pitch C
_d      = pitch D
_e      = pitch E
_f      = pitch F
_g      = pitch G
_a      = pitch A
_b      = pitch B


-- pch'c

-- * Relative octaves (6.1.6)

--------------------------------------------------------------------------------
-- * Rests (6.1.9)

-- | @r1@ - whole rest.
r1 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r1      = rest # dur 1

-- | @r2@ - half rest.
r2 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r2      = rest # dur 2

-- | @r4@ - quarter rest.
r4 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r4      = rest # dur 4

-- | @r8@ - 8th rest.
r8 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r8      = rest # dur 8

-- | @r16@ - 16th rest.
r16 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r16     = rest # dur 16

-- | @r4@ - 32nd rest.
r32 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r32     = rest # dur 32

-- | @r4@ - 64th rest.
r64 :: (SymRest repr, SymDuration repr, SymAttrDuration repr) => repr (Rest ctx)
r64     = rest # dur 64

--------------------------------------------------------------------------------
-- * Skips (6.1.10)

-- | @\\skip@.
skip :: (SymCmdSkip repr) => repr (CmdSkip ctx) 
skip = cmdSkip "skip"


-- | @s1@ - whole skip.
s1 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s1      = skipDuration $ duration 1

-- | @s2@ - half skip.
s2 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s2      = skipDuration $ duration 2

-- | @s4@ - quarter skip.
s4 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s4      = skipDuration $ duration 4

-- | @s8@ - 8th skip.
s8 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s8      = skipDuration $ duration 8

-- | @s16@ - 16th skip.
s16 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s16     = skipDuration $ duration 16

-- | @s32@ - 32nd skip.
s32 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s32     = skipDuration $ duration 32

-- | @s64@ - 64th skip.
s64 :: (SymSkipDuration repr, SymDuration repr) => repr (SkipDuration ctx)
s64     = skipDuration $ duration 64

--------------------------------------------------------------------------------
-- * Durations (6.2.1)
dur :: (SymAttrDuration repr, AttrDuration a, SymDuration repr) =>
       Int -> repr (a ctx) -> repr (a ctx)
dur i = attrduration $ duration i


-- | @\\longa@.
longa :: (AttrCmdLongDuration a, SymAttrCmdLongDuration repr) => 
         repr (a Ctx_Element) -> repr (a Ctx_Element) 
longa   = cmdLongDuration "longa"  

-- | @\\breve@.
breve :: (AttrCmdLongDuration a, SymAttrCmdLongDuration repr) => 
         repr (a Ctx_Element) -> repr (a Ctx_Element) 
breve   = cmdLongDuration "breve"

--------------------------------------------------------------------------------
-- * Augmentation dots (6.2.2)
dot :: (SymAttrDuration repr, AttrDuration a, 
        SymDuration repr, SymAttrDotted repr) =>
       Int -> repr (a ctx) -> repr (a ctx)
dot i = attrduration $ dotted 1 $ duration i


dotdot :: (SymAttrDuration repr, AttrDuration a, 
           SymDuration repr, SymAttrDotted repr) =>
          Int -> repr (a ctx) -> repr (a ctx)
dotdot i = attrduration $ dotted 2 $ duration i

--------------------------------------------------------------------------------
-- * Stems (6.3.2)
-- | @\\stemUp@.
stemUp                  :: (SymCmdStem repr) => repr (CmdStem Ctx_Note)  
stemUp                  = cmdStem "stemUp"  

-- | @\\stemDown@.
stemDown                :: (SymCmdStem repr) => repr (CmdStem Ctx_Note)  
stemDown                = cmdStem "stemDown"    

-- | @\\stemNeutral@.
stemNeutral             :: (SymCmdStem repr) => repr (CmdStem Ctx_Note)  
stemNeutral             = cmdStem "stemNeutral"  

--------------------------------------------------------------------------------
-- * Clef (6.4.1)

-- | @treble@.
treble          :: SymClefType repr => repr (ClefType ctx)
treble          = cleftype "treble"

-- | @alto@.
alto            :: SymClefType repr => repr (ClefType ctx)
alto            = cleftype "alto"

-- | @tenor@.
tenor           :: SymClefType repr => repr (ClefType ctx)
tenor           = cleftype "tenor"

-- | @bass@.
bass            :: SymClefType repr => repr (ClefType ctx)
bass            = cleftype "bass"

-- | @french@.
french          :: SymClefType repr => repr (ClefType ctx)
french          = cleftype "french"

-- | @soprano@.
soprano         :: SymClefType repr => repr (ClefType ctx)
soprano         = cleftype "soprano"

-- | @mezzosoprano@.
mezzosoprano    :: SymClefType repr => repr (ClefType ctx)
mezzosoprano    = cleftype "mezzosoprano"

-- | @baritone@.
baritone        :: SymClefType repr => repr (ClefType ctx)
baritone        = cleftype "baritone"

-- | @varbaritone@.
varbaritone     :: SymClefType repr => repr (ClefType ctx)
varbaritone     = cleftype "varbaritone"

-- | @subbass@.
subbass         :: SymClefType repr => repr (ClefType ctx)
subbass         = cleftype "subbass" 

-- | @percussion@.
percussion      :: SymClefType repr => repr (ClefType ctx)
percussion      = cleftype "percussion" 

-- | @tabClef@.
tabClef         :: SymClefType repr => repr (ClefType ctx)
tabClef         = cleftype "tabClef" 

-- | Clef attribute - transpose a clef up an octave.
clefUp8     :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefUp8     = clefTransposition 8

-- | Clef attribute - transpose a clef up two octaves.
clefUp15    :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefUp15    = clefTransposition 15

-- | Clef attribute - transpose a clef down an octave.
clefDown8   :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefDown8   = clefTransposition (-8)

-- | Clef attribute - transpose a clef down two octaves.
clefDown15  :: (AttrClefTransposition a, SymAttrClefTransposition repr) => 
               repr (a ctx) -> repr (a ctx)
clefDown15  = clefTransposition (-15)

--------------------------------------------------------------------------------  
-- * Key signature (6.4.2)

-- | @\\major@.
major           :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element)  
major           = keyType "major"

-- | @\\minor@.
minor           :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
minor           = keyType "minor"

-- | @\\ionian@.
ionian          :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
ionian          = keyType "ionian"

-- | @\\locrian@.
locrian         :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
locrian         = keyType "locrian"

-- | @\\aeolian@.
aeolian         :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
aeolian         = keyType "aeolian"

-- | @\\mixolydian@.
mixolydian      :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
mixolydian      = keyType "mixolydian"

-- | @\\lydian@.
lydian          :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
lydian          = keyType "lydian"

-- | @\\phrygian@.
phrygian        :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
phrygian        = keyType "phrygian"

-- | @\\dorian@.
dorian          :: (SymCmdKeyType repr) => repr (CmdKeyType Ctx_Element) 
dorian          = keyType "dorian"

--------------------------------------------------------------------------------
-- * Time signature (6.4.3)

--------------------------------------------------------------------------------
-- * Bar lines (6.4.5)

--------------------------------------------------------------------------------
-- * Unmetered music (6.4.6)

-- | @\\cadenzaOn@.
cadenzaOn     :: (SymCmdCadenza repr) => repr (CmdCadenza ctx)
cadenzaOn     = cmdCadenza "cadenzaOn"

-- | @\\cadenzaOff@.
cadenzaOff    :: (SymCmdCadenza repr) => repr (CmdCadenza ctx)
cadenzaOff    = cmdCadenza "cadenzaOff"

--------------------------------------------------------------------------------  
-- * Ties (6.5.1)

-- | @\\repeatTie@.
repeatTie               :: (SymCmdTie repr) => repr (CmdTie Ctx_Note)   
repeatTie               = cmdTie "repeatTie" 

-- | @\\tieUp@.
tieUp           :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
tieUp           = cmdTie "tieUp"

-- | @\\tieDown@.
tieDown         :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
tieDown         = cmdTie "tieDown"

-- | @\\tieNeutral@.
tieNeutral      :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
tieNeutral      = cmdTie "tieNeutral"

-- | @\\tieDotted@.
tieDotted       :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
tieDotted       = cmdTie "tieDotted"

-- | @\\tieDashed@.
tieDashed       :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
tieDashed       = cmdTie "tieDashed"

-- | @\\tieSolid@.
tieSolid        :: (SymCmdTie repr) => repr (CmdTie Ctx_Note) 
tieSolid        = cmdTie "tieSolid"

--------------------------------------------------------------------------------
-- * Slurs (6.5.2)

-- | @\\slurUp@.
slurUp          :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurUp          = cmdSlur "slurUp"

-- | @\\slurDown@.
slurDown        :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurDown        = cmdSlur "slurDown"

-- | @\\slurNeutral@.
slurNeutral     :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurNeutral     = cmdSlur "slurNeutral"

-- | @\\slurDashed@.
slurDashed      :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurDashed      = cmdSlur "slurDashed"

-- | @\\slurDotted@.
slurDotted      :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurDotted      = cmdSlur "slurDotted"

-- | @\\slurSolid@.
slurSolid       :: (SymCmdSlur repr) => repr (CmdSlur ctx)
slurSolid       = cmdSlur "slurSolid"

--------------------------------------------------------------------------------
-- * Phrasing slurs (6.5.3)

-- | @\\(@.
openPhrasingSlur      :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx)
openPhrasingSlur      = cmdPhrasingSlur "("

-- | @\\)@.
closePhrasingSlur     :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx)
closePhrasingSlur     = cmdPhrasingSlur ")"

-- | @\\phrasingSlurUp@.
phrasingSlurUp        :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx)
phrasingSlurUp        = cmdPhrasingSlur "phrasingSlurUp"

-- | @\\phrasingSlurDown@.
phrasingSlurDown      :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx)
phrasingSlurDown      = cmdPhrasingSlur "phrasingSlurDown"

-- | @\\phrasingSlurNeutral@.
phrasingSlurNeutral   :: SymCmdPhrasingSlur repr => repr (CmdPhrasingSlur ctx)
phrasingSlurNeutral   = cmdPhrasingSlur "phrasingSlurNeutral"
  
--------------------------------------------------------------------------------
-- * Grace notes (6.5.7)

-- | @\\grace@.
grace         :: SymCmdGrace repr => repr (CmdGrace ctx)
grace         = cmdGrace "grace"

-- | @\\acciaccatura@.
acciaccatura  :: SymCmdGrace repr => repr (CmdGrace ctx)
acciaccatura  = cmdGrace "acciaccatura"

-- | @\\appoggiatura@.
appoggiatura  :: SymCmdGrace repr => repr (CmdGrace ctx)
appoggiatura  = cmdGrace "appoggiatura" 
  
--------------------------------------------------------------------------------
-- * Articulations (6.6.1)

-- | @\\accent@.
accent                  :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)           
accent                  = attr $ cmdArticulation "accent"  

-- | @\\marcato@.
marcato                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
marcato                 = attr $ cmdArticulation "marcato" 

-- | @\\staccatissimo@.
staccatissimo           :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
staccatissimo           = attr $ cmdArticulation "staccatissimo" 

-- | @\\espressivo@.
espressivo              :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
espressivo              = attr $ cmdArticulation "espressivo"  

-- | @\\staccato@.
staccato                :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
staccato                = attr $ cmdArticulation "staccato" 

-- | @\\tenuto@.
tenuto                  :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
tenuto                  = attr $ cmdArticulation "tenuto" 

-- | @\\portato@.
portato                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
portato                 = attr $ cmdArticulation "portato"

-- | @\\upbow@.
upbow                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
upbow                   = attr $ cmdArticulation "upbow"

-- | @\\downbow@.
downbow                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
downbow                 = attr $ cmdArticulation "downbow" 

-- | @\\flageolet@.
flageolet               :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
flageolet               = attr $ cmdArticulation "flageolet" 

-- | @\\thumb@.
thumb                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
thumb                   = attr $ cmdArticulation "thumb" 

-- | @\\lheel@.
lheel                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
lheel                   = attr $ cmdArticulation "lheel" 

-- | @\\rheel@.
rheel                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
rheel                   = attr $ cmdArticulation "rheel" 

-- | @\\ltoe@.
ltoe                    :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
ltoe                    = attr $ cmdArticulation "ltoe" 

-- | @\\rtoe@.
rtoe                    :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
rtoe                    = attr $ cmdArticulation "rtoe" 

-- | @\\open@.
open                    :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
open                    = attr $ cmdArticulation "open"

-- | @\\stopped@.
stopped                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
stopped                 = attr $ cmdArticulation "stopped"

-- | @\\turn@.
turn                    :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
turn                    = attr $ cmdArticulation "turn" 

-- | @\\reverseturn@.
reverseturn             :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
reverseturn             = attr $ cmdArticulation "reverseturn"      

-- | @\\trill@.
trill                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
trill                   = attr $ cmdArticulation "trill"  

-- | @\\prall@.
prall                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
prall                   = attr $ cmdArticulation "prall"  

-- | @\\mordent@.
mordent                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
mordent                 = attr $ cmdArticulation "mordent"   

-- | @\\prallprall@.
prallprall              :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
prallprall              = attr $ cmdArticulation "prallprall" 

-- | @\\prallmordent@.
prallmordent            :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
prallmordent            = attr $ cmdArticulation "prallmordent"  

-- | @\\upprall@.
upprall                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
upprall                 = attr $ cmdArticulation "upprall"   

-- | @\\downprall@.
downprall               :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
downprall               = attr $ cmdArticulation "downprall" 

-- | @\\upmordent@.
upmordent               :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
upmordent               = attr $ cmdArticulation "upmordent"  

-- | @\\downmordent@.
downmordent             :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
downmordent             = attr $ cmdArticulation "downmordent" 

-- | @\\pralldown@.
pralldown               :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
pralldown               = attr $ cmdArticulation "pralldown"   

-- | @\\prallup@.
prallup                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
prallup                 = attr $ cmdArticulation "prallup"  

-- | @\\lineprall@.
lineprall               :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
lineprall               = attr $ cmdArticulation "lineprall" 

-- | @\\signumcongruentiae@.
signumcongruentiae      :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
signumcongruentiae      = attr $ cmdArticulation "signumcongruentiae"  

-- | @\\shortfermata@.
shortfermata            :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
shortfermata            = attr $ cmdArticulation "shortfermata"  

-- | @\\fermata@.
fermata                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
fermata                 = attr $ cmdArticulation "fermata" 

-- | @\\longfermata@.
longfermata             :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
longfermata             = attr $ cmdArticulation "longfermata" 

-- | @\\verylongfermata@.
verylongfermata         :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
verylongfermata         = attr $ cmdArticulation "verylongfermata"    

-- | @\\segno@.
segno                   :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element)
segno                   = attr $ cmdArticulation "segno" 

-- | @\\coda@.
coda                    :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element) 
coda                    = attr $ cmdArticulation "coda" 

-- | @\\varcoda@.
varcoda                 :: (Attr repr, SymCmdArticulation repr) => 
                           repr (b Ctx_Element) -> repr (b Ctx_Element) 
varcoda                 = attr $ cmdArticulation "varcoda" 


--------------------------------------------------------------------------------
-- * Dynamics (6.6.3)
-- nullary commands (use underscore suffix _ for commands)

-- | @\\ppppp@.
ppppp_                  :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
ppppp_                  = cmdDynamic "ppppp" 

-- | @\\pppp@.
pppp_                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
pppp_                   = cmdDynamic "pppp" 

-- | @\\ppp@.
ppp_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
ppp_                    = cmdDynamic "ppp" 

-- | @\\p@.
pp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
pp_                     = cmdDynamic "pp" 

-- | @\\p@ - renamed piano.
piano                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
piano                   = cmdDynamic "p" 

-- | @\\mp@.
mp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
mp_                     = cmdDynamic "mp" 

-- | @\\mf@.
mf_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
mf_                     = cmdDynamic "mf" 

-- | @\\f@ - renamed forte.
forte                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
forte                   = cmdDynamic "f"

-- | @\\ff@.
ff_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
ff_                     = cmdDynamic "ff"

-- | @\\fff@.
fff_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
fff_                    = cmdDynamic "fff"

-- | @\\ffff@.
ffff_                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
ffff_                   = cmdDynamic "ffff"

-- | @\\fp@.
fp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
fp_                     = cmdDynamic "fp"

-- | @\\sf@.
sf_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sf_                     = cmdDynamic "sf"

-- | @\\sff@.
sff_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sff_                    = cmdDynamic "sff"

-- | @\\sp@.
sp_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sp_                     = cmdDynamic "sp"

-- | @\\spp@.
spp_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
spp_                    = cmdDynamic "spp"

-- | @\\sfz@.
sfz_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
sfz_                    = cmdDynamic "sfz"

-- | @\\rfz@.
rfz_                    :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)  
rfz_                    = cmdDynamic "rfz"

-- | @\\<@.
openCrescendo           :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
openCrescendo           = cmdDynamic "<"

-- | @\\>@.
openDecrescendo         :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
openDecrescendo         = cmdDynamic ">"
 
-- | @\\!@. 
closeDynamic            :: (SymCmdDynamic repr) => repr (CmdDynamic ctx) 
closeDynamic            = cmdDynamic "!"

-- | @\\cr@ - alias of \\<. 
cr_                     :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
cr_                     = cmdDynamic "cr" 

-- | @\\decr@ - alias of \\>. 
decr_                   :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
decr_                   = cmdDynamic "decr"   

-- | @\\dynamicUp@. 
dynamicUp               :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
dynamicUp               = cmdDynamic "dynamicUp"  

-- | @\\dynamicDown@. 
dynamicDown             :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
dynamicDown             = cmdDynamic "dynamicDown"  

-- | @\\dynamicNeutral@. 
dynamicNeutral          :: (SymCmdDynamic repr) => repr (CmdDynamic ctx)
dynamicNeutral          = cmdDynamic "dynamicNeutral"  

  
--------------------------------------------------------------------------------
-- * Breath marks (6.6.4)
-- | @\\breathe@.
breathe       :: SymCmdBreathe repr => repr (CmdBreathe ctx) 
breathe       = cmdBreathe "breathe"

--------------------------------------------------------------------------------
-- * Glissando (6.6.6)
-- | @\\glissando@.
glissando :: SymCmdGlissando repr => repr (CmdGlissando ctx)
glissando = cmdGlissando "glissando"  

--------------------------------------------------------------------------------
-- * Arpeggio (6.6.7)

-- | @\\arpeggio@.
arpeggio          :: SymCmdArpeggio repr => repr (CmdArpeggio ctx)
arpeggio          = cmdArpeggio "arpeggio"

-- | @\\arpeggioUp@.
arpeggioUp        :: SymCmdArpeggio repr => repr (CmdArpeggio ctx)
arpeggioUp        = cmdArpeggio "arpeggioUp"

-- | @\\arpeggioDown@.
arpeggioDown      :: SymCmdArpeggio repr => repr (CmdArpeggio ctx)
arpeggioDown      = cmdArpeggio "arpeggioDown"

-- | @\\arpeggioNeutral@.
arpeggioNeutral   :: SymCmdArpeggio repr => repr (CmdArpeggio ctx)
arpeggioNeutral   = cmdArpeggio "arpeggioNeutral"  

-- | @\\arpeggioBracket@.
arpeggioBracket   :: SymCmdArpeggio repr => repr (CmdArpeggio ctx)
arpeggioBracket   = cmdArpeggio "arpeggioBracket"
  
--------------------------------------------------------------------------------  
-- * Falls and doits (6.6.8)

--------------------------------------------------------------------------------
-- * Metronome marks (8.8.2)

--------------------------------------------------------------------------------
-- * Creating contexts (9.2.2)

staff :: (SymContextType repr) => repr (ContextType ctx)
staff = contextType "Staff"

voice :: (SymContextType repr) => repr (ContextType ctx)
voice = contextType "Voice"

tabStaff :: (SymContextType repr) => repr (ContextType ctx)
tabStaff = contextType "TabStaff"

--------------------------------------------------------------------------------
-- * Multiple scores in a book (10.1.2)


--------------------------------------------------------------------------------
-- * Titles and headers (10.2)

--------------------------------------------------------------------------------
-- * Creating titles (10.2.1)

