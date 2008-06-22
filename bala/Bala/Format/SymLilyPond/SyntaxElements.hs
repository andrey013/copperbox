{-# LANGUAGE FlexibleContexts #-}

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


--------------------------------------------------------------------------------
-- * Contexts for lists

elementCtx :: (SymCList repr CT_Element) => repr (CList CT_Element)
elementCtx = cNil

toplevelCtx :: (SymCList repr CT_Toplevel) => repr (CList CT_Toplevel)
toplevelCtx = cNil



-- comments and versioning (2.12)

--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)

_c, _d, _e, _f, _g , _a, _b :: (SymPitch repr) => repr (Pitch ctx)
_c      = pitch C
_d      = pitch D
_e      = pitch E
_f      = pitch F
_g      = pitch G
_a      = pitch A
_b      = pitch B



-- *** Relative octaves (6.1.6)

--------------------------------------------------------------------------------
-- *** Rests (6.1.9)

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
-- *** Skips (6.1.10)

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
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)
dur :: (SymAttrDuration repr, AttrDuration a, SymDuration repr) 
    => Int -> repr (a ctx) -> repr (a ctx)
dur i = attrduration $ duration i


-- | @\\longa@.
longa :: (AttrCmdLongDuration a, SymAttrCmdLongDuration repr) 
      => repr (a CT_Element) -> repr (a CT_Element) 
longa   = cmdLongDuration "longa"  

-- | @\\breve@.
breve :: (AttrCmdLongDuration a, SymAttrCmdLongDuration repr)
      => repr (a CT_Element) -> repr (a CT_Element) 
breve   = cmdLongDuration "breve"

--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)
dot :: (SymAttrDuration repr, AttrDuration a, 
        SymDuration repr, SymAttrDotted repr) 
    => Int -> repr (a ctx) -> repr (a ctx)
dot i = attrduration $ dotted 1 $ duration i


dotdot :: (SymAttrDuration repr, AttrDuration a, 
           SymDuration repr, SymAttrDotted repr) 
       => Int -> repr (a ctx) -> repr (a ctx)
dotdot i = attrduration $ dotted 2 $ duration i

--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)


times :: SymCmdTimes repr
      => MeterFraction -> repr (CList ctxa) -> repr (CmdTimes ctxb)
times = cmdTimes  
  
--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Stems (6.3.2)
-- | @\\stemUp@.
stemUp                  :: (SymCmdStem repr) => repr (CmdStem CT_Note)  
stemUp                  = cmdStem "stemUp"  

-- | @\\stemDown@.
stemDown                :: (SymCmdStem repr) => repr (CmdStem CT_Note)  
stemDown                = cmdStem "stemDown"    

-- | @\\stemNeutral@.
stemNeutral             :: (SymCmdStem repr) => repr (CmdStem CT_Note)  
stemNeutral             = cmdStem "stemNeutral"  

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

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
clefUp8     :: (AttrClefTransposition a, SymAttrClefTransposition repr) 
            => repr (a ctx) -> repr (a ctx)
clefUp8     = clefTransposition 8

-- | Clef attribute - transpose a clef up two octaves.
clefUp15    :: (AttrClefTransposition a, SymAttrClefTransposition repr)
            => repr (a ctx) -> repr (a ctx)
clefUp15    = clefTransposition 15

-- | Clef attribute - transpose a clef down an octave.
clefDown8   :: (AttrClefTransposition a, SymAttrClefTransposition repr)
            => repr (a ctx) -> repr (a ctx)
clefDown8   = clefTransposition (-8)

-- | Clef attribute - transpose a clef down two octaves.
clefDown15  :: (AttrClefTransposition a, SymAttrClefTransposition repr)
            => repr (a ctx) -> repr (a ctx)
clefDown15  = clefTransposition (-15)

--------------------------------------------------------------------------------  
-- *** Key signature (6.4.2)

-- | @\\major@.
major           :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element)  
major           = keyType "major"

-- | @\\minor@.
minor           :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
minor           = keyType "minor"

-- | @\\ionian@.
ionian          :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
ionian          = keyType "ionian"

-- | @\\locrian@.
locrian         :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
locrian         = keyType "locrian"

-- | @\\aeolian@.
aeolian         :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
aeolian         = keyType "aeolian"

-- | @\\mixolydian@.
mixolydian      :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
mixolydian      = keyType "mixolydian"

-- | @\\lydian@.
lydian          :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
lydian          = keyType "lydian"

-- | @\\phrygian@.
phrygian        :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
phrygian        = keyType "phrygian"

-- | @\\dorian@.
dorian          :: (SymCmdKeyType repr) => repr (CmdKeyType CT_Element) 
dorian          = keyType "dorian"

--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

-- | @\\cadenzaOn@.
cadenzaOn     :: (SymCmdCadenza repr) => repr (CmdCadenza ctx)
cadenzaOn     = cmdCadenza "cadenzaOn"

-- | @\\cadenzaOff@.
cadenzaOff    :: (SymCmdCadenza repr) => repr (CmdCadenza ctx)
cadenzaOff    = cmdCadenza "cadenzaOff"

--------------------------------------------------------------------------------  
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

-- | @\\repeatTie@.
repeatTie               :: (SymCmdTie repr) => repr (CmdTie CT_Note)   
repeatTie               = cmdTie "repeatTie" 

-- | @\\tieUp@.
tieUp           :: (SymCmdTie repr) => repr (CmdTie CT_Note) 
tieUp           = cmdTie "tieUp"

-- | @\\tieDown@.
tieDown         :: (SymCmdTie repr) => repr (CmdTie CT_Note) 
tieDown         = cmdTie "tieDown"

-- | @\\tieNeutral@.
tieNeutral      :: (SymCmdTie repr) => repr (CmdTie CT_Note) 
tieNeutral      = cmdTie "tieNeutral"

-- | @\\tieDotted@.
tieDotted       :: (SymCmdTie repr) => repr (CmdTie CT_Note) 
tieDotted       = cmdTie "tieDotted"

-- | @\\tieDashed@.
tieDashed       :: (SymCmdTie repr) => repr (CmdTie CT_Note) 
tieDashed       = cmdTie "tieDashed"

-- | @\\tieSolid@.
tieSolid        :: (SymCmdTie repr) => repr (CmdTie CT_Note) 
tieSolid        = cmdTie "tieSolid"

--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

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
-- *** Phrasing slurs (6.5.3)

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
-- *** Grace notes (6.5.7)

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
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1)

-- | @\\accent@.
accent                  :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element)           
accent                  = attr $ cmdArticulation "accent"  

-- | @\\marcato@.
marcato                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
marcato                 = attr $ cmdArticulation "marcato" 

-- | @\\staccatissimo@.
staccatissimo           :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element)  
staccatissimo           = attr $ cmdArticulation "staccatissimo" 

-- | @\\espressivo@.
espressivo              :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element)   
espressivo              = attr $ cmdArticulation "espressivo"  

-- | @\\staccato@.
staccato                :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
staccato                = attr $ cmdArticulation "staccato" 

-- | @\\tenuto@.
tenuto                  :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
tenuto                  = attr $ cmdArticulation "tenuto" 

-- | @\\portato@.
portato                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
portato                 = attr $ cmdArticulation "portato"

-- | @\\upbow@.
upbow                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
upbow                   = attr $ cmdArticulation "upbow"

-- | @\\downbow@.
downbow                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
downbow                 = attr $ cmdArticulation "downbow" 

-- | @\\flageolet@.
flageolet               :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
flageolet               = attr $ cmdArticulation "flageolet" 

-- | @\\thumb@.
thumb                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
thumb                   = attr $ cmdArticulation "thumb" 

-- | @\\lheel@.
lheel                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
lheel                   = attr $ cmdArticulation "lheel" 

-- | @\\rheel@.
rheel                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
rheel                   = attr $ cmdArticulation "rheel" 

-- | @\\ltoe@.
ltoe                    :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
ltoe                    = attr $ cmdArticulation "ltoe" 

-- | @\\rtoe@.
rtoe                    :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
rtoe                    = attr $ cmdArticulation "rtoe" 

-- | @\\open@.
open                    :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
open                    = attr $ cmdArticulation "open"

-- | @\\stopped@.
stopped                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
stopped                 = attr $ cmdArticulation "stopped"

-- | @\\turn@.
turn                    :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
turn                    = attr $ cmdArticulation "turn" 

-- | @\\reverseturn@.
reverseturn             :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
reverseturn             = attr $ cmdArticulation "reverseturn"      

-- | @\\trill@.
trill                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
trill                   = attr $ cmdArticulation "trill"  

-- | @\\prall@.
prall                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
prall                   = attr $ cmdArticulation "prall"  

-- | @\\mordent@.
mordent                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
mordent                 = attr $ cmdArticulation "mordent"   

-- | @\\prallprall@.
prallprall              :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
prallprall              = attr $ cmdArticulation "prallprall" 

-- | @\\prallmordent@.
prallmordent            :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
prallmordent            = attr $ cmdArticulation "prallmordent"  

-- | @\\upprall@.
upprall                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
upprall                 = attr $ cmdArticulation "upprall"   

-- | @\\downprall@.
downprall               :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
downprall               = attr $ cmdArticulation "downprall" 

-- | @\\upmordent@.
upmordent               :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
upmordent               = attr $ cmdArticulation "upmordent"  

-- | @\\downmordent@.
downmordent             :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
downmordent             = attr $ cmdArticulation "downmordent" 

-- | @\\pralldown@.
pralldown               :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
pralldown               = attr $ cmdArticulation "pralldown"   

-- | @\\prallup@.
prallup                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
prallup                 = attr $ cmdArticulation "prallup"  

-- | @\\lineprall@.
lineprall               :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
lineprall               = attr $ cmdArticulation "lineprall" 

-- | @\\signumcongruentiae@.
signumcongruentiae      :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
signumcongruentiae      = attr $ cmdArticulation "signumcongruentiae"  

-- | @\\shortfermata@.
shortfermata            :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
shortfermata            = attr $ cmdArticulation "shortfermata"  

-- | @\\fermata@.
fermata                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
fermata                 = attr $ cmdArticulation "fermata" 

-- | @\\longfermata@.
longfermata             :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
longfermata             = attr $ cmdArticulation "longfermata" 

-- | @\\verylongfermata@.
verylongfermata         :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
verylongfermata         = attr $ cmdArticulation "verylongfermata"    

-- | @\\segno@.
segno                   :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
segno                   = attr $ cmdArticulation "segno" 

-- | @\\coda@.
coda                    :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
coda                    = attr $ cmdArticulation "coda" 

-- | @\\varcoda@.
varcoda                 :: (Attr repr, SymCmdArticulation repr) 
                        => repr (a CT_Element) -> repr (a CT_Element) 
varcoda                 = attr $ cmdArticulation "varcoda" 


--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)
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
-- *** Breath marks (6.6.4)
-- | @\\breathe@.
breathe       :: SymCmdBreathe repr => repr (CmdBreathe ctx) 
breathe       = cmdBreathe "breathe"

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)
-- | @\\glissando@.
glissando :: SymCmdGlissando repr => repr (CmdGlissando ctx)
glissando = cmdGlissando "glissando"  

--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

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
-- *** Falls and doits (6.6.8)


--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

-- *** Pedals (7.1.2)  
-- | @\\sustainDown@.
sustainDown       :: (AttrCmdPedal a, SymAttrCmdPedal repr)
                  => repr (a CT_Element) -> repr (a CT_Element) 
sustainDown       = cmdPedal "sustainDown"  

-- | @\\sustainUp@.
sustainUp         :: (AttrCmdPedal a, SymAttrCmdPedal repr)
                  => repr (a CT_Element) -> repr (a CT_Element)  
sustainUp         = cmdPedal "sustainUp"  

-- | @\\unaCorda@.
unaCorda          :: (AttrCmdPedal a, SymAttrCmdPedal repr)
                  => repr (a CT_Element) -> repr (a CT_Element)  
unaCorda          = cmdPedal "unaCorda"  

-- | @\\treCorde@.
treCorde          :: (AttrCmdPedal a, SymAttrCmdPedal repr)
                  => repr (a CT_Element) -> repr (a CT_Element) 
treCorde          = cmdPedal "treCorde"  

-- | @\\sostenutoDown@.
sostenutoDown   :: (AttrCmdPedal a, SymAttrCmdPedal repr)
                  => repr (a CT_Element) -> repr (a CT_Element)  
sostenutoDown   = cmdPedal "sostenutoDown"  

-- | @\\sostenutoUp@.
sostenutoUp       :: (AttrCmdPedal a, SymAttrCmdPedal repr)
                  => repr (a CT_Element) -> repr (a CT_Element)  
sostenutoUp       = cmdPedal "sostenutoUp"  


--------------------------------------------------------------------------------
-- ** Chord names (7.2)
-- *** Chords mode (7.2.2)

--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)
-- *** Showing melody rhythms(7.4.1)

--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

-- | @acousticbassdrum@. 
acousticbassdrum      :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
acousticbassdrum      = drumPitchName "acousticbassdrum"

-- | @bassdrum@. 
bassdrum              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bassdrum              = drumPitchName "bassdrum"

-- | @hisidestick@. 
hisidestick           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hisidestick           = drumPitchName "hisidestick"

-- | @sidestick@. 
sidestick             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
sidestick             = drumPitchName "sidestick"

-- | @losidestick@. 
losidestick           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
losidestick           = drumPitchName "losidestick"

-- | @acousticsnare@. 
acousticsnare         :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
acousticsnare         = drumPitchName "acousticbassdrum"

-- | @snare@. 
snare                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
snare                 = drumPitchName "snare"

-- | @handclap@.
handclap              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
handclap              = drumPitchName "handclap"

-- | @electricsnare@.
electricsnare         :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
electricsnare         = drumPitchName "electricsnare"

-- | @lowfloortom@.
lowfloortom           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
lowfloortom           = drumPitchName "lowfloortom"

-- | @closedhihat@.
closedhihat           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
closedhihat           = drumPitchName "closedhihat"

-- | @hihat@.
hihat                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hihat                 = drumPitchName "hihat"

-- | @highfloortom@.
highfloortom          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
highfloortom          = drumPitchName "highfloortom"

-- | @pedalhihat@.
pedalhihat            :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
pedalhihat            = drumPitchName "pedalhihat"

-- | @lowtom@.
lowtom                :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
lowtom                = drumPitchName "lowtom"

-- | @openhihat@.
openhihat             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
openhihat             = drumPitchName "openhihat"

-- | @halfopenhihat@.
halfopenhihat         :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
halfopenhihat         = drumPitchName "halfopenhihat"

-- | @lowmidtom@.
lowmidtom             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
lowmidtom             = drumPitchName "lowmidtom"

-- | @himidtom@.
himidtom              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
himidtom              = drumPitchName "himidtom"

-- | @crashcymbala@.
crashcymbala          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
crashcymbala          = drumPitchName "crashcymbala"

-- | @crashcymbal@.
crashcymbal           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
crashcymbal           = drumPitchName "crashcymbal"

-- | @hightom@.
hightom               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hightom               = drumPitchName "hightom"

-- | @ridecymbala@.

ridecymbala           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ridecymbala           = drumPitchName "ridecymbala"

-- | @ridecymbal@.
ridecymbal            :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ridecymbal            = drumPitchName "ridecymbal"

-- | @chinesecymbal@.
chinesecymbal         :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
chinesecymbal         = drumPitchName "chinesecymbal"

-- | @ridebell@.
ridebell              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ridebell              = drumPitchName "ridebell"

-- | @tambourine@.
tambourine            :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tambourine            = drumPitchName "tambourine"

-- | @splashcymbal@.
splashcymbal          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
splashcymbal          = drumPitchName "splashcymbal"

-- | @cowbell@.
cowbell               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cowbell               = drumPitchName "cowbell"

-- | @crashcymbalb@.
crashcymbalb          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
crashcymbalb          = drumPitchName "crashcymbalb"

-- | @vibraslap@.
vibraslap             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
vibraslap             = drumPitchName "vibraslap"

-- | @ridecymbalb@.
ridecymbalb           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ridecymbalb           = drumPitchName "ridecymbalb"

-- | @mutehibongo@.
mutehibongo           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
mutehibongo           = drumPitchName "mutehibongo"

-- | @hibongo@.
hibongo               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hibongo               = drumPitchName "hibongo"

-- | @openhibongo@.
openhibongo           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
openhibongo           = drumPitchName "openhibongo"

-- | @mutelobongo@.
mutelobongo           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
mutelobongo           = drumPitchName "mutelobongo"

-- | @lobongo@.
lobongo               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
lobongo               = drumPitchName "lobongo"

-- | @openlobongo@.
openlobongo           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
openlobongo           = drumPitchName "openlobongo"

-- | @mutehiconga@.
mutehiconga           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
mutehiconga           = drumPitchName "mutehiconga"

-- | @muteloconga@.
muteloconga           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
muteloconga           = drumPitchName "muteloconga"

-- | @openhiconga@.
openhiconga           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
openhiconga           = drumPitchName "openhiconga"

-- | @hiconga@.
hiconga               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hiconga               = drumPitchName "hiconga"

-- | @openloconga@.
openloconga           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
openloconga           = drumPitchName "openloconga"

-- | @loconga@.
loconga               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
loconga               = drumPitchName "loconga"

-- | @hitimbale@.
hitimbale             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hitimbale             = drumPitchName "hitimbale"

-- | @lotimbale@.
lotimbale             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
lotimbale             = drumPitchName "lotimbale"

-- | @hiagogo@.
hiagogo               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hiagogo               = drumPitchName "hiagogo"

-- | @loagogo@.
loagogo               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
loagogo               = drumPitchName "loagogo"

-- | @cabasa@.
cabasa                :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cabasa                = drumPitchName "cabasa"

-- | @maracas@.
maracas               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
maracas               = drumPitchName "maracas"

-- | @shortwhistle@.
shortwhistle          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
shortwhistle          = drumPitchName "shortwhistle"

-- | @longwhistle@.
longwhistle           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
longwhistle           = drumPitchName "longwhistle"

-- | @shortguiro@.
shortguiro            :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
shortguiro            = drumPitchName "shortguiro"

-- | @longguiro@.
longguiro             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
longguiro             = drumPitchName "longguiro"

-- | @guiro@.
guiro                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
guiro                 = drumPitchName "guiro"

-- | @claves@.
claves                :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
claves                = drumPitchName "claves"

-- | @hiwoodblock@.
hiwoodblock           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hiwoodblock           = drumPitchName "hiwoodblock"

-- | @lowoodblock@.
lowoodblock           :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
lowoodblock           = drumPitchName "lowoodblock"

-- | @mutecuica@.
mutecuica             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
mutecuica             = drumPitchName "mutecuica"

-- | @opencuica@.
opencuica             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
opencuica             = drumPitchName "opencuica"

-- | @mutetriangle@.
mutetriangle          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
mutetriangle          = drumPitchName "mutetriangle"

-- | @triangle@.
triangle              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
triangle              = drumPitchName "triangle"

-- | @opentriangle@.
opentriangle          :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
opentriangle          = drumPitchName "opentriangle"

-- | @oneup@.
oneup                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
oneup                 = drumPitchName "oneup"

-- | @twoup@.
twoup                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
twoup                 = drumPitchName "twoup"

-- | @threeup@.
threeup               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
threeup               = drumPitchName "threeup"

-- | @fourup@.
fourup                :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
fourup                = drumPitchName "fourup"

-- | @fiveup@.
fiveup                :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
fiveup                = drumPitchName "fiveup"

-- | @onedown@.
onedown               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
onedown               = drumPitchName "onedown"

-- | @twodown@.
twodown               :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
twodown               = drumPitchName "twodown"

-- | @threedown@.
threedown             :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
threedown             = drumPitchName "threedown"

-- | @fourdown@.
fourdown              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
fourdown              = drumPitchName "fourdown"

-- | @fivedown@.
fivedown              :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
fivedown              = drumPitchName "fivedown"
   


-- | @bda@ - abbreviated name for 'acousticbassdrum'. 
bda                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bda                   = drumPitchName "bda"

-- | @bd@ - abbreviated name for 'bassdrum'.
bd                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bd                    = drumPitchName "bd"

-- | @ssh@ - abbreviated name for 'hisidestick'.
ssh                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ssh                   = drumPitchName "ssh"

-- | @ss@ - abbreviated name for 'sidestick'.
ss                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ss                    = drumPitchName "ss"

-- | @ssl@ - abbreviated name for 'losidestick'.
ssl                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ssl                   = drumPitchName "ssl"

-- | @sna@ - abbreviated name for 'acousticsnare'.
sna                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
sna                   = drumPitchName "sna"

-- | @sn@ - abbreviated name for 'snare'.
sn                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
sn                    = drumPitchName "sn"

-- | @hc@ - abbreviated name for 'handclap'.
hc                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hc                    = drumPitchName "hc"

-- | @sne@ - abbreviated name for 'electricsnare'.
sne                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
sne                   = drumPitchName "sne"

-- | @tomfl@ - abbreviated name for 'lowfloortom'.
tomfl                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tomfl                 = drumPitchName "tomfl"

-- | @hhc@ - abbreviated name for 'closedhihat'.
hhc                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hhc                   = drumPitchName "hhc"

-- | @hh@ - abbreviated name for 'hihat'.
hh                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hh                    = drumPitchName "hh"

-- | @tomfh@ - abbreviated name for 'highfloortom'.
tomfh                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tomfh                 = drumPitchName "tomfh"


-- | @hhp@ - abbreviated name for 'pedalhihat'.
hhp                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hhp                   = drumPitchName "hhp"

-- | @toml@ - abbreviated name for 'lowtom'.
toml                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
toml                  = drumPitchName "toml"

-- | @hho@ - abbreviated name for 'openhihat'.
hho                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hho                   = drumPitchName "hho"

-- | @hhho@ - abbreviated name for 'halfopenhihat'.
hhho                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
hhho                  = drumPitchName "hhho"

-- | @tomml@ - abbreviated name for 'lowmidtom'.
tomml                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tomml                 = drumPitchName "tomml"

-- | @tommh@ - abbreviated name for 'himidtom'.
tommh                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tommh                 = drumPitchName "tommh"

-- | @cymca@ - abbreviated name for 'crashcymbala'.
cymca                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymca                 = drumPitchName "cymca"

-- | @cymc@ - abbreviated name for 'crashcymbal'.
cymc                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymc                  = drumPitchName "cymc"

-- | @tomh@ - abbreviated name for 'hightom'.
tomh                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tomh                  = drumPitchName "tomh"

-- | @cymra@ - abbreviated name for 'ridecymbala'.
cymra                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymra                 = drumPitchName "cymra"

-- | @cymr@ - abbreviated name for 'ridecymbal'.
cymr                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymr                  = drumPitchName "cymr"

-- | @cymch@ - abbreviated name for 'chinesecymbal'.
cymch                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymch                 = drumPitchName "cymch"

-- | @rb@ - abbreviated name for 'ridebell'.
rb                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
rb                    = drumPitchName "rb"

-- | @tamb@ - abbreviated name for 'tambourine'.
tamb                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tamb                  = drumPitchName "tamb"

-- | @cyms@ - abbreviated name for 'splashcymbal'.
cyms                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cyms                  = drumPitchName "cyms"

-- | @cb@ - abbreviated name for 'cowbell'.
cb                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cb                    = drumPitchName "cb"

-- | @cymcb@ - abbreviated name for 'crashcymbalb'.
cymcb                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymcb                 = drumPitchName "cymcb"

-- | @vibs@ - abbreviated name for 'vibraslap'.
vibs                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
vibs                  = drumPitchName "vibs"

-- | @cymrb@ - abbreviated name for 'ridecymbalb'.
cymrb                 :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cymrb                 = drumPitchName "cymrb"

-- | @bohm@ - abbreviated name for 'mutehibongo'.
bohm                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bohm                  = drumPitchName "bohm"

-- | @boh@ - abbreviated name for 'hibongo'.
boh                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
boh                   = drumPitchName "boh"

-- | @boho@ - abbreviated name for 'openhibongo'.
boho                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
boho                  = drumPitchName "boho"

-- | @bolm@ - abbreviated name for 'mutelobongo'.
bolm                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bolm                  = drumPitchName "bolm"

-- | @bol@ - abbreviated name for 'lobongo'.
bol                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bol                   = drumPitchName "bol"

-- | @bolo@ - abbreviated name for 'openlobongo'.
bolo                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
bolo                  = drumPitchName "bolo"

-- | @cghm@ - abbreviated name for 'mutehiconga'.
cghm                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cghm                  = drumPitchName "cghm"

-- | @cglm@ - abbreviated name for 'muteloconga'.
cglm                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cglm                  = drumPitchName "cglm"

-- | @cgho@ - abbreviated name for 'openhiconga'.
cgho                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cgho                  = drumPitchName "cgho"

-- | @cgh@ - abbreviated name for 'hiconga'.
cgh                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cgh                   = drumPitchName "cgh"

-- | @cglo@ - abbreviated name for 'openloconga'.
cglo                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cglo                  = drumPitchName "cglo"

-- | @cgl@ - abbreviated name for 'loconga'.
cgl                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cgl                   = drumPitchName "cgl"

-- | @timh@ - abbreviated name for 'hitimbale'.
timh                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
timh                  = drumPitchName "timh"

-- | @timl@ - abbreviated name for 'lotimbale'.
timl                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
timl                  = drumPitchName "timl"

-- | @agh@ - abbreviated name for 'hiagogo'.
agh                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
agh                   = drumPitchName "agh"

-- | @agl@ - abbreviated name for 'loagogo'.
agl                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
agl                   = drumPitchName "agl"

-- | @cab@ - abbreviated name for 'cabasa'.
cab                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cab                   = drumPitchName "cab"

-- | @mar@ - abbreviated name for 'maracas'.
mar                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
mar                   = drumPitchName "mar"

-- | @whs@ - abbreviated name for 'shortwhistle'.
whs                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
whs                   = drumPitchName "whs"

-- | @whl@ - abbreviated name for 'longwhistle'.
whl                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
whl                   = drumPitchName "whl"

-- | @guis@ - abbreviated name for 'shortguiro'.
guis                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
guis                  = drumPitchName "guis"

-- | @guil@ - abbreviated name for 'longguiro'.
guil                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
guil                  = drumPitchName "guil"

-- | @gui@ - abbreviated name for 'guiro'.
gui                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
gui                   = drumPitchName "gui"

-- | @cl@ - abbreviated name for 'claves'.
cl                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cl                    = drumPitchName "cl"

-- | @wbh@ - abbreviated name for 'hiwoodblock'.
wbh                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
wbh                   = drumPitchName "wbh"

-- | @wbl@ - abbreviated name for 'lowoodblock'.
wbl                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
wbl                   = drumPitchName "wbl"

-- | @cuim@ - abbreviated name for 'mutecuica'.
cuim                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cuim                  = drumPitchName "cuim"

-- | @cuio@ - abbreviated name for 'opencuica'.
cuio                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
cuio                  = drumPitchName "cuio"


-- | @trim@ - abbreviated name for 'mutetriangle'.
trim                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
trim                  = drumPitchName "trim"

-- | @tri@ - abbreviated name for 'triangle'.
tri                   :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tri                   = drumPitchName "tri"

-- | @trio@ - abbreviated name for 'opentriangle'.
trio                  :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
trio                  = drumPitchName "trio"

-- | @tt@ - abbreviated name for 'tamtam'.
tt                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
tt                    = drumPitchName "tt"

-- | @ua@ - abbreviated name for 'oneup'.
ua                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ua                    = drumPitchName "ua"

-- | @ub@ - abbreviated name for 'twoup'.
ub                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ub                    = drumPitchName "ub"

-- | @uc@ - abbreviated name for 'threeup'.
uc                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
uc                    = drumPitchName "uc"

-- | @ud@ - abbreviated name for 'fourup'.
ud                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ud                    = drumPitchName "ud"

-- | @ue@ - abbreviated name for 'fiveup'.
ue                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
ue                    = drumPitchName "ue"

-- | @da@ - abbreviated name for 'onedown'.
da                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
da                    = drumPitchName "da"

-- | @db@ - abbreviated name for 'twodown'.
db                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
db                    = drumPitchName "db"

-- | @dc@ - abbreviated name for 'threedown'.
dc                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
dc                    = drumPitchName "dc"

-- | @dd@ - abbreviated name for 'fourdown'.
dd                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
dd                    = drumPitchName "dd"

-- | @de@ - abbreviated name for 'fivedown'.
de                    :: (SymDrumPitchName repr) => repr (DrumPitchName ctx)
de                    = drumPitchName "de"

--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)
-- *** Right hand fingerings (7.5.6)

--------------------------------------------------------------------------------
-- ** Other instrument specific notation (7.8)
-- *** Artificial harmonics (strings) (7.8.1)


--------------------------------------------------------------------------------
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

-- *** Text markup (8.1.4)


-- ** Preparing parts (8.2)
-- *** Metronome marks (8.8.2)

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)


--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)
-- *** Multiple scores in a book (10.1.2)


--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)
-- *** Creating titles (10.2.1)


--------------------------------------------------------------------------------    
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

