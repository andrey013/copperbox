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

elementCtx    :: (CSnocList repr CT_Element) => repr (SnocList CT_Element)
elementCtx    = snil

toplevelCtx   :: (CSnocList repr CT_Toplevel) => repr (SnocList CT_Toplevel)
toplevelCtx   = snil

headerCtx     :: (CSnocList repr CT_Header) => repr (SnocList CT_Header)
headerCtx     = snil


bookCtx       :: (CSnocList repr CT_Book) => repr (SnocList CT_Book)
bookCtx       = snil



-- comments and versioning (2.12)

--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1)

_c, _d, _e, _f, _g , _a, _b :: (CPitch repr) => repr Pitch
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
r1 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r1      = rest `attr` duration 1

-- | @r2@ - half rest.
r2 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r2      = rest `attr` duration 2

-- | @r4@ - quarter rest.
r4 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r4      = rest %% duration 4

-- | @r8@ - 8th rest.
r8 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r8      = rest %% duration 8

-- | @r16@ - 16th rest.
r16 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r16     = rest %% duration 16

-- | @r4@ - 32nd rest.
r32 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r32     = rest %% duration 32

-- | @r4@ - 64th rest.
r64 :: (CRest repr, CDuration repr, CAttr repr) => repr Rest
r64     = rest %% duration 64

--------------------------------------------------------------------------------
-- *** Skips (6.1.10)

-- | @\\skip@.
skip :: (CCmdSkip repr) => repr CmdSkip 
skip = cmdSkip "skip"


-- | @s1@ - whole skip.
s1 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s1      = skipDuration $ duration 1

-- | @s2@ - half skip.
s2 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s2      = skipDuration $ duration 2

-- | @s4@ - quarter skip.
s4 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s4      = skipDuration $ duration 4

-- | @s8@ - 8th skip.
s8 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s8      = skipDuration $ duration 8

-- | @s16@ - 16th skip.
s16 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s16     = skipDuration $ duration 16

-- | @s32@ - 32nd skip.
s32 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s32     = skipDuration $ duration 32

-- | @s64@ - 64th skip.
s64 :: (CSkipDuration repr, CDuration repr) => repr SkipDuration
s64     = skipDuration $ duration 64

--------------------------------------------------------------------------------
-- ** Rhythms (6.2)
-- *** Durations (6.2.1)

dur :: (CDuration repr) => Int -> repr Duration
dur = duration



-- breve and longa could be members of CDuration

-- | @\\longa@.
longa   :: (CCmdLongDuration repr) => repr CmdLongDuration
longa   = cmdLongDuration "longa"  

-- | @\\breve@.
breve   :: (CCmdLongDuration repr) => repr CmdLongDuration
breve   = cmdLongDuration "breve"



--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)
dot :: (CDotted repr) => repr Dotted
dot = dotted 1


dotdot :: (CDotted repr) => repr Dotted
dotdot = dotted 2

--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)


times :: (CCmdTimes repr)
      => MeterFraction -> repr (SnocList ctx) -> repr CmdTimes
times = cmdTimes  
  
--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Stems (6.3.2)
-- | @\\stemUp@.
stemUp                  :: (CCmdStem repr) => repr CmdStem
stemUp                  = cmdStem "stemUp"  

-- | @\\stemDown@.
stemDown                :: (CCmdStem repr) => repr CmdStem
stemDown                = cmdStem "stemDown"    

-- | @\\stemNeutral@.
stemNeutral             :: (CCmdStem repr) => repr CmdStem
stemNeutral             = cmdStem "stemNeutral"  

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)

-- | @treble@.
treble          :: CClefType repr => repr ClefType
treble          = cleftype "treble"

-- | @alto@.
alto            :: CClefType repr => repr ClefType
alto            = cleftype "alto"

-- | @tenor@.
tenor           :: CClefType repr => repr ClefType
tenor           = cleftype "tenor"

-- | @bass@.
bass            :: CClefType repr => repr ClefType
bass            = cleftype "bass"

-- | @french@.
french          :: CClefType repr => repr ClefType
french          = cleftype "french"

-- | @soprano@.
soprano         :: CClefType repr => repr ClefType
soprano         = cleftype "soprano"

-- | @mezzosoprano@.
mezzosoprano    :: CClefType repr => repr ClefType
mezzosoprano    = cleftype "mezzosoprano"

-- | @baritone@.
baritone        :: CClefType repr => repr ClefType
baritone        = cleftype "baritone"

-- | @varbaritone@.
varbaritone     :: CClefType repr => repr ClefType
varbaritone     = cleftype "varbaritone"

-- | @subbass@.
subbass         :: CClefType repr => repr ClefType
subbass         = cleftype "subbass" 

-- | @percussion@.
percussion      :: CClefType repr => repr ClefType
percussion      = cleftype "percussion" 

-- | @tabClef@.
tabClef         :: CClefType repr => repr ClefType
tabClef         = cleftype "tabClef" 

-- | Clef attribute - transpose a clef up an octave.
clefUp8     :: (CClefTransposition repr) => repr ClefTransposition
clefUp8     = clefTransposition 8

-- | Clef attribute - transpose a clef up two octaves.
clefUp15    :: (CClefTransposition repr) => repr ClefTransposition
clefUp15    = clefTransposition 15

-- | Clef attribute - transpose a clef down an octave.
clefDown8   :: (CClefTransposition repr) => repr ClefTransposition
clefDown8   = clefTransposition (-8)

-- | Clef attribute - transpose a clef down two octaves.
clefDown15  :: (CClefTransposition repr) => repr ClefTransposition
clefDown15  = clefTransposition (-15)

--------------------------------------------------------------------------------  
-- *** Key signature (6.4.2)

-- | @\\major@.
major           :: (CCmdKeyType repr) => repr CmdKeyType
major           = keyType "major"

-- | @\\minor@.
minor           :: (CCmdKeyType repr) => repr CmdKeyType
minor           = keyType "minor"

-- | @\\ionian@.
ionian          :: (CCmdKeyType repr) => repr CmdKeyType
ionian          = keyType "ionian"

-- | @\\locrian@.
locrian         :: (CCmdKeyType repr) => repr CmdKeyType
locrian         = keyType "locrian"

-- | @\\aeolian@.
aeolian         :: (CCmdKeyType repr) => repr CmdKeyType
aeolian         = keyType "aeolian"

-- | @\\mixolydian@.
mixolydian      :: (CCmdKeyType repr) => repr CmdKeyType
mixolydian      = keyType "mixolydian"

-- | @\\lydian@.
lydian          :: (CCmdKeyType repr) => repr CmdKeyType
lydian          = keyType "lydian"

-- | @\\phrygian@.
phrygian        :: (CCmdKeyType repr) => repr CmdKeyType
phrygian        = keyType "phrygian"

-- | @\\dorian@.
dorian          :: (CCmdKeyType repr) => repr CmdKeyType
dorian          = keyType "dorian"

--------------------------------------------------------------------------------
-- *** Time signature (6.4.3)

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

-- | @\\cadenzaOn@.
cadenzaOn     :: (CCmdCadenza repr) => repr CmdCadenza
cadenzaOn     = cmdCadenza "cadenzaOn"

-- | @\\cadenzaOff@.
cadenzaOff    :: (CCmdCadenza repr) => repr CmdCadenza
cadenzaOff    = cmdCadenza "cadenzaOff"

--------------------------------------------------------------------------------  
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)

-- | @\\repeatTie@.
repeatTie       :: (CCmdTie repr) => repr CmdTie  
repeatTie       = cmdTie "repeatTie" 

-- | @\\tieUp@.
tieUp           :: (CCmdTie repr) => repr CmdTie
tieUp           = cmdTie "tieUp"

-- | @\\tieDown@.
tieDown         :: (CCmdTie repr) => repr CmdTie
tieDown         = cmdTie "tieDown"

-- | @\\tieNeutral@.
tieNeutral      :: (CCmdTie repr) => repr CmdTie
tieNeutral      = cmdTie "tieNeutral"

-- | @\\tieDotted@.
tieDotted       :: (CCmdTie repr) => repr CmdTie
tieDotted       = cmdTie "tieDotted"

-- | @\\tieDashed@.
tieDashed       :: (CCmdTie repr) => repr CmdTie
tieDashed       = cmdTie "tieDashed"

-- | @\\tieSolid@.
tieSolid        :: (CCmdTie repr) => repr CmdTie
tieSolid        = cmdTie "tieSolid"

--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)

-- | @\\slurUp@.
slurUp          :: (CCmdSlur repr) => repr CmdSlur
slurUp          = cmdSlur "slurUp"

-- | @\\slurDown@.
slurDown        :: (CCmdSlur repr) => repr CmdSlur
slurDown        = cmdSlur "slurDown"

-- | @\\slurNeutral@.
slurNeutral     :: (CCmdSlur repr) => repr CmdSlur
slurNeutral     = cmdSlur "slurNeutral"

-- | @\\slurDashed@.
slurDashed      :: (CCmdSlur repr) => repr CmdSlur
slurDashed      = cmdSlur "slurDashed"

-- | @\\slurDotted@.
slurDotted      :: (CCmdSlur repr) => repr CmdSlur
slurDotted      = cmdSlur "slurDotted"

-- | @\\slurSolid@.
slurSolid       :: (CCmdSlur repr) => repr CmdSlur
slurSolid       = cmdSlur "slurSolid"

--------------------------------------------------------------------------------
-- *** Phrasing slurs (6.5.3)

-- | @\\(@.
openPhrasingSlur      :: (CCmdPhrasingSlur repr) => repr CmdPhrasingSlur
openPhrasingSlur      = cmdPhrasingSlur "("

-- | @\\)@.
closePhrasingSlur     :: (CCmdPhrasingSlur repr) => repr CmdPhrasingSlur
closePhrasingSlur     = cmdPhrasingSlur ")"

-- | @\\phrasingSlurUp@.
phrasingSlurUp        :: (CCmdPhrasingSlur repr) => repr CmdPhrasingSlur
phrasingSlurUp        = cmdPhrasingSlur "phrasingSlurUp"

-- | @\\phrasingSlurDown@.
phrasingSlurDown      :: (CCmdPhrasingSlur repr) => repr CmdPhrasingSlur
phrasingSlurDown      = cmdPhrasingSlur "phrasingSlurDown"

-- | @\\phrasingSlurNeutral@.
phrasingSlurNeutral   :: (CCmdPhrasingSlur repr) => repr CmdPhrasingSlur
phrasingSlurNeutral   = cmdPhrasingSlur "phrasingSlurNeutral"
  
--------------------------------------------------------------------------------
-- *** Grace notes (6.5.7)

-- | @\\grace@.
grace         :: CCmdGrace repr => repr CmdGrace
grace         = cmdGrace "grace"

-- | @\\acciaccatura@.
acciaccatura  :: CCmdGrace repr => repr CmdGrace
acciaccatura  = cmdGrace "acciaccatura"

-- | @\\appoggiatura@.
appoggiatura  :: CCmdGrace repr => repr CmdGrace
appoggiatura  = cmdGrace "appoggiatura" 
  
--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1)

-- | Place a mark above the note with @^@.
vabove    :: (CVerticalPlacement repr, CPrefixAttr repr,
              PrefixAttribute a VerticalPlacement) 
          => repr a -> repr a
vabove    = prefixAttr (verticalPlacement VAbove)


-- | Place a mark below the note with @_@.
vbelow    :: (CVerticalPlacement repr, CPrefixAttr repr,
              PrefixAttribute a VerticalPlacement) 
          => repr a -> repr a
vbelow    = prefixAttr (verticalPlacement VAbove)


-- | Place a mark about the note with @-@. The renderer will decide where 
-- it goes.
vdefault  :: (CVerticalPlacement repr, CPrefixAttr repr,
              PrefixAttribute a VerticalPlacement) 
          => repr a -> repr a
vdefault  = prefixAttr (verticalPlacement VAbove)



-- | @-^@ - dashHat, aka @marcato@.
dashHat           :: (CArticulation repr) => repr Articulation
dashHat           = articulation "-^"

-- | @-+@ - dashPlus, aka @stopped@.
dashPlus          :: (CArticulation repr) => repr Articulation
dashPlus          = articulation "-+"

-- | @--@ - dashDash, aka @tenuto@.
dashDash          :: (CArticulation repr) => repr Articulation
dashDash          = articulation "--"

-- | @-|@ - dashBar, aka @staccatissimo@.
dashBar           :: (CArticulation repr) => repr Articulation
dashBar           = articulation "-|"

-- | @->@ - dashLarger, aka @accent@.
dashLarger        :: (CArticulation repr) => repr Articulation
dashLarger        = articulation "->"

-- | @-.@ - dashDot, aka @staccato@.
dashDot           :: (CArticulation repr) => repr Articulation
dashDot           = articulation "-."

-- | @-_@ - dashUnderscore, aka @portato@.
dashUnderscore    :: (CArticulation repr) => repr Articulation
dashUnderscore    = articulation "-_"



-- | @\\accent@.
accent                  :: (CCmdArticulation repr) => repr CmdArticulation          
accent                  = cmdArticulation "accent"  

-- | @\\marcato@.
marcato                 :: (CCmdArticulation repr) => repr CmdArticulation
marcato                 = cmdArticulation "marcato" 

-- | @\\staccatissimo@.
staccatissimo           :: (CCmdArticulation repr) => repr CmdArticulation
staccatissimo           = cmdArticulation "staccatissimo" 

-- | @\\espressivo@.
espressivo              :: (CCmdArticulation repr) => repr CmdArticulation  
espressivo              = cmdArticulation "espressivo"  

-- | @\\staccato@.
staccato                :: (CCmdArticulation repr) => repr CmdArticulation
staccato                = cmdArticulation "staccato" 

-- | @\\tenuto@.
tenuto                  :: (CCmdArticulation repr) => repr CmdArticulation
tenuto                  = cmdArticulation "tenuto" 

-- | @\\portato@.
portato                 :: (CCmdArticulation repr) => repr CmdArticulation
portato                 = cmdArticulation "portato"

-- | @\\upbow@.
upbow                   :: (CCmdArticulation repr) => repr CmdArticulation
upbow                   = cmdArticulation "upbow"

-- | @\\downbow@.
downbow                 :: (CCmdArticulation repr) => repr CmdArticulation
downbow                 = cmdArticulation "downbow" 

-- | @\\flageolet@.
flageolet               :: (CCmdArticulation repr) => repr CmdArticulation
flageolet               = cmdArticulation "flageolet" 

-- | @\\thumb@.
thumb                   :: (CCmdArticulation repr) => repr CmdArticulation
thumb                   = cmdArticulation "thumb" 

-- | @\\lheel@.
lheel                   :: (CCmdArticulation repr) => repr CmdArticulation
lheel                   = cmdArticulation "lheel" 

-- | @\\rheel@.
rheel                   :: (CCmdArticulation repr) => repr CmdArticulation
rheel                   = cmdArticulation "rheel" 

-- | @\\ltoe@.
ltoe                    :: (CCmdArticulation repr) => repr CmdArticulation
ltoe                    = cmdArticulation "ltoe" 

-- | @\\rtoe@.
rtoe                    :: (CCmdArticulation repr) => repr CmdArticulation
rtoe                    = cmdArticulation "rtoe" 

-- | @\\open@.
open                    :: (CCmdArticulation repr) => repr CmdArticulation
open                    = cmdArticulation "open"

-- | @\\stopped@.
stopped                 :: (CCmdArticulation repr) => repr CmdArticulation
stopped                 = cmdArticulation "stopped"

-- | @\\turn@.
turn                    :: (CCmdArticulation repr) => repr CmdArticulation
turn                    = cmdArticulation "turn" 

-- | @\\reverseturn@.
reverseturn             :: (CCmdArticulation repr) => repr CmdArticulation
reverseturn             = cmdArticulation "reverseturn"      

-- | @\\trill@.
trill                   :: (CCmdArticulation repr) => repr CmdArticulation
trill                   = cmdArticulation "trill"  

-- | @\\prall@.
prall                   :: (CCmdArticulation repr) => repr CmdArticulation
prall                   = cmdArticulation "prall"  

-- | @\\mordent@.
mordent                 :: (CCmdArticulation repr) => repr CmdArticulation
mordent                 = cmdArticulation "mordent"   

-- | @\\prallprall@.
prallprall              :: (CCmdArticulation repr) => repr CmdArticulation
prallprall              = cmdArticulation "prallprall" 

-- | @\\prallmordent@.
prallmordent            :: (CCmdArticulation repr) => repr CmdArticulation
prallmordent            = cmdArticulation "prallmordent"  

-- | @\\upprall@.
upprall                 :: (CCmdArticulation repr) => repr CmdArticulation
upprall                 = cmdArticulation "upprall"   

-- | @\\downprall@.
downprall               :: (CCmdArticulation repr) => repr CmdArticulation
downprall               = cmdArticulation "downprall" 

-- | @\\upmordent@.
upmordent               :: (CCmdArticulation repr) => repr CmdArticulation
upmordent               = cmdArticulation "upmordent"  

-- | @\\downmordent@.
downmordent             :: (CCmdArticulation repr) => repr CmdArticulation
downmordent             = cmdArticulation "downmordent" 

-- | @\\pralldown@.
pralldown               :: (CCmdArticulation repr) => repr CmdArticulation
pralldown               = cmdArticulation "pralldown"   

-- | @\\prallup@.
prallup                 :: (CCmdArticulation repr) => repr CmdArticulation
prallup                 = cmdArticulation "prallup"  

-- | @\\lineprall@.
lineprall               :: (CCmdArticulation repr) => repr CmdArticulation
lineprall               = cmdArticulation "lineprall" 

-- | @\\signumcongruentiae@.
signumcongruentiae      :: (CCmdArticulation repr) => repr CmdArticulation
signumcongruentiae      = cmdArticulation "signumcongruentiae"  

-- | @\\shortfermata@.
shortfermata            :: (CCmdArticulation repr) => repr CmdArticulation
shortfermata            = cmdArticulation "shortfermata"  

-- | @\\fermata@.
fermata                 :: (CCmdArticulation repr) => repr CmdArticulation
fermata                 = cmdArticulation "fermata" 

-- | @\\longfermata@.
longfermata             :: (CCmdArticulation repr) => repr CmdArticulation
longfermata             = cmdArticulation "longfermata" 

-- | @\\verylongfermata@.
verylongfermata         :: (CCmdArticulation repr) => repr CmdArticulation 
verylongfermata         = cmdArticulation "verylongfermata"    

-- | @\\segno@.
segno                   :: (CCmdArticulation repr) => repr CmdArticulation
segno                   = cmdArticulation "segno" 

-- | @\\coda@.
coda                    :: (CCmdArticulation repr) => repr CmdArticulation
coda                    = cmdArticulation "coda" 

-- | @\\varcoda@.
varcoda                 :: (CCmdArticulation repr) => repr CmdArticulation
varcoda                 = cmdArticulation "varcoda" 


--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)
-- nullary commands (use underscore suffix _ for commands)

-- | @\\ppppp@.
ppppp_                  :: (CCmdDynamic repr) => repr CmdDynamic
ppppp_                  = cmdDynamic "ppppp" 

-- | @\\pppp@.
pppp_                   :: (CCmdDynamic repr) => repr CmdDynamic
pppp_                   = cmdDynamic "pppp" 

-- | @\\ppp@.
ppp_                    :: (CCmdDynamic repr) => repr CmdDynamic
ppp_                    = cmdDynamic "ppp" 

-- | @\\pp@.
pp_                     :: (CCmdDynamic repr) => repr CmdDynamic
pp_                     = cmdDynamic "pp" 

-- | @\\p@ - renamed piano.
piano                   :: (CCmdDynamic repr) => repr CmdDynamic
piano                   = cmdDynamic "p" 

-- | @\\mp@.
mp_                     :: (CCmdDynamic repr) => repr CmdDynamic
mp_                     = cmdDynamic "mp" 

-- | @\\mf@.
mf_                     :: (CCmdDynamic repr) => repr CmdDynamic
mf_                     = cmdDynamic "mf" 

-- | @\\f@ - renamed forte.
forte                   :: (CCmdDynamic repr) => repr CmdDynamic
forte                   = cmdDynamic "f"

-- | @\\ff@.
ff_                     :: (CCmdDynamic repr) => repr CmdDynamic
ff_                     = cmdDynamic "ff"

-- | @\\fff@.
fff_                    :: (CCmdDynamic repr) => repr CmdDynamic
fff_                    = cmdDynamic "fff"

-- | @\\ffff@.
ffff_                   :: (CCmdDynamic repr) => repr CmdDynamic
ffff_                   = cmdDynamic "ffff"

-- | @\\fp@.
fp_                     :: (CCmdDynamic repr) => repr CmdDynamic
fp_                     = cmdDynamic "fp"

-- | @\\sf@.
sf_                     :: (CCmdDynamic repr) => repr CmdDynamic
sf_                     = cmdDynamic "sf"

-- | @\\sff@.
sff_                    :: (CCmdDynamic repr) => repr CmdDynamic
sff_                    = cmdDynamic "sff"

-- | @\\sp@.
sp_                     :: (CCmdDynamic repr) => repr CmdDynamic
sp_                     = cmdDynamic "sp"

-- | @\\spp@.
spp_                    :: (CCmdDynamic repr) => repr CmdDynamic
spp_                    = cmdDynamic "spp"

-- | @\\sfz@.
sfz_                    :: (CCmdDynamic repr) => repr CmdDynamic
sfz_                    = cmdDynamic "sfz"

-- | @\\rfz@.
rfz_                    :: (CCmdDynamic repr) => repr CmdDynamic
rfz_                    = cmdDynamic "rfz"

-- | @\\<@.
openCrescendo           :: (CCmdDynamic repr) => repr CmdDynamic
openCrescendo           = cmdDynamic "<"

-- | @\\>@.
openDecrescendo         :: (CCmdDynamic repr) => repr CmdDynamic
openDecrescendo         = cmdDynamic ">"
 
-- | @\\!@. 
closeDynamic            :: (CCmdDynamic repr) => repr CmdDynamic 
closeDynamic            = cmdDynamic "!"

-- | @\\cr@ - alias of \\<. 
cr_                     :: (CCmdDynamic repr) => repr CmdDynamic
cr_                     = cmdDynamic "cr" 

-- | @\\decr@ - alias of \\>. 
decr_                   :: (CCmdDynamic repr) => repr CmdDynamic
decr_                   = cmdDynamic "decr"   

-- | @\\dynamicUp@. 
dynamicUp               :: (CCmdDynamic repr) => repr CmdDynamic
dynamicUp               = cmdDynamic "dynamicUp"  

-- | @\\dynamicDown@. 
dynamicDown             :: (CCmdDynamic repr) => repr CmdDynamic
dynamicDown             = cmdDynamic "dynamicDown"  

-- | @\\dynamicNeutral@. 
dynamicNeutral          :: (CCmdDynamic repr) => repr CmdDynamic
dynamicNeutral          = cmdDynamic "dynamicNeutral"  

  
--------------------------------------------------------------------------------
-- *** Breath marks (6.6.4)
-- | @\\breathe@.
breathe       :: CCmdBreathe repr => repr CmdBreathe
breathe       = cmdBreathe "breathe"

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)
-- | @\\glissando@.
glissando :: CCmdGlissando repr => repr CmdGlissando
glissando = cmdGlissando "glissando"  

--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

-- | @\\arpeggio@.
arpeggio          :: CCmdArpeggio repr => repr CmdArpeggio
arpeggio          = cmdArpeggio "arpeggio"

-- | @\\arpeggioUp@.
arpeggioUp        :: CCmdArpeggio repr => repr CmdArpeggio
arpeggioUp        = cmdArpeggio "arpeggioUp"

-- | @\\arpeggioDown@.
arpeggioDown      :: CCmdArpeggio repr => repr CmdArpeggio
arpeggioDown      = cmdArpeggio "arpeggioDown"

-- | @\\arpeggioNeutral@.
arpeggioNeutral   :: CCmdArpeggio repr => repr CmdArpeggio
arpeggioNeutral   = cmdArpeggio "arpeggioNeutral"  

-- | @\\arpeggioBracket@.
arpeggioBracket   :: CCmdArpeggio repr => repr CmdArpeggio
arpeggioBracket   = cmdArpeggio "arpeggioBracket"
  
--------------------------------------------------------------------------------  
-- *** Falls and doits (6.6.8)


--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

-- *** Pedals (7.1.2)  
-- | @\\sustainDown@.
sustainDown       :: (CCmdPedal repr) => repr CmdPedal
sustainDown       = cmdPedal "sustainDown"  

-- | @\\sustainUp@.
sustainUp         :: (CCmdPedal repr) => repr CmdPedal 
sustainUp         = cmdPedal "sustainUp"  

-- | @\\unaCorda@.
unaCorda          :: (CCmdPedal repr) => repr CmdPedal 
unaCorda          = cmdPedal "unaCorda"  

-- | @\\treCorde@.
treCorde          :: (CCmdPedal repr) => repr CmdPedal 
treCorde          = cmdPedal "treCorde"  

-- | @\\sostenutoDown@.
sostenutoDown     :: (CCmdPedal repr) => repr CmdPedal 
sostenutoDown     = cmdPedal "sostenutoDown"  

-- | @\\sostenutoUp@.
sostenutoUp       :: (CCmdPedal repr) => repr CmdPedal 
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
acousticbassdrum      :: (CDrumPitchName repr) => repr DrumPitchName
acousticbassdrum      = drumPitchName "acousticbassdrum"

-- | @bassdrum@. 
bassdrum              :: (CDrumPitchName repr) => repr DrumPitchName
bassdrum              = drumPitchName "bassdrum"

-- | @hisidestick@. 
hisidestick           :: (CDrumPitchName repr) => repr DrumPitchName
hisidestick           = drumPitchName "hisidestick"

-- | @sidestick@. 
sidestick             :: (CDrumPitchName repr) => repr DrumPitchName
sidestick             = drumPitchName "sidestick"

-- | @losidestick@. 
losidestick           :: (CDrumPitchName repr) => repr DrumPitchName
losidestick           = drumPitchName "losidestick"

-- | @acousticsnare@. 
acousticsnare         :: (CDrumPitchName repr) => repr DrumPitchName
acousticsnare         = drumPitchName "acousticbassdrum"

-- | @snare@. 
snare                 :: (CDrumPitchName repr) => repr DrumPitchName
snare                 = drumPitchName "snare"

-- | @handclap@.
handclap              :: (CDrumPitchName repr) => repr DrumPitchName
handclap              = drumPitchName "handclap"

-- | @electricsnare@.
electricsnare         :: (CDrumPitchName repr) => repr DrumPitchName
electricsnare         = drumPitchName "electricsnare"

-- | @lowfloortom@.
lowfloortom           :: (CDrumPitchName repr) => repr DrumPitchName
lowfloortom           = drumPitchName "lowfloortom"

-- | @closedhihat@.
closedhihat           :: (CDrumPitchName repr) => repr DrumPitchName
closedhihat           = drumPitchName "closedhihat"

-- | @hihat@.
hihat                 :: (CDrumPitchName repr) => repr DrumPitchName
hihat                 = drumPitchName "hihat"

-- | @highfloortom@.
highfloortom          :: (CDrumPitchName repr) => repr DrumPitchName
highfloortom          = drumPitchName "highfloortom"

-- | @pedalhihat@.
pedalhihat            :: (CDrumPitchName repr) => repr DrumPitchName
pedalhihat            = drumPitchName "pedalhihat"

-- | @lowtom@.
lowtom                :: (CDrumPitchName repr) => repr DrumPitchName
lowtom                = drumPitchName "lowtom"

-- | @openhihat@.
openhihat             :: (CDrumPitchName repr) => repr DrumPitchName
openhihat             = drumPitchName "openhihat"

-- | @halfopenhihat@.
halfopenhihat         :: (CDrumPitchName repr) => repr DrumPitchName
halfopenhihat         = drumPitchName "halfopenhihat"

-- | @lowmidtom@.
lowmidtom             :: (CDrumPitchName repr) => repr DrumPitchName
lowmidtom             = drumPitchName "lowmidtom"

-- | @himidtom@.
himidtom              :: (CDrumPitchName repr) => repr DrumPitchName
himidtom              = drumPitchName "himidtom"

-- | @crashcymbala@.
crashcymbala          :: (CDrumPitchName repr) => repr DrumPitchName
crashcymbala          = drumPitchName "crashcymbala"

-- | @crashcymbal@.
crashcymbal           :: (CDrumPitchName repr) => repr DrumPitchName
crashcymbal           = drumPitchName "crashcymbal"

-- | @hightom@.
hightom               :: (CDrumPitchName repr) => repr DrumPitchName
hightom               = drumPitchName "hightom"

-- | @ridecymbala@.

ridecymbala           :: (CDrumPitchName repr) => repr DrumPitchName
ridecymbala           = drumPitchName "ridecymbala"

-- | @ridecymbal@.
ridecymbal            :: (CDrumPitchName repr) => repr DrumPitchName
ridecymbal            = drumPitchName "ridecymbal"

-- | @chinesecymbal@.
chinesecymbal         :: (CDrumPitchName repr) => repr DrumPitchName
chinesecymbal         = drumPitchName "chinesecymbal"

-- | @ridebell@.
ridebell              :: (CDrumPitchName repr) => repr DrumPitchName
ridebell              = drumPitchName "ridebell"

-- | @tambourine@.
tambourine            :: (CDrumPitchName repr) => repr DrumPitchName
tambourine            = drumPitchName "tambourine"

-- | @splashcymbal@.
splashcymbal          :: (CDrumPitchName repr) => repr DrumPitchName
splashcymbal          = drumPitchName "splashcymbal"

-- | @cowbell@.
cowbell               :: (CDrumPitchName repr) => repr DrumPitchName
cowbell               = drumPitchName "cowbell"

-- | @crashcymbalb@.
crashcymbalb          :: (CDrumPitchName repr) => repr DrumPitchName
crashcymbalb          = drumPitchName "crashcymbalb"

-- | @vibraslap@.
vibraslap             :: (CDrumPitchName repr) => repr DrumPitchName
vibraslap             = drumPitchName "vibraslap"

-- | @ridecymbalb@.
ridecymbalb           :: (CDrumPitchName repr) => repr DrumPitchName
ridecymbalb           = drumPitchName "ridecymbalb"

-- | @mutehibongo@.
mutehibongo           :: (CDrumPitchName repr) => repr DrumPitchName
mutehibongo           = drumPitchName "mutehibongo"

-- | @hibongo@.
hibongo               :: (CDrumPitchName repr) => repr DrumPitchName
hibongo               = drumPitchName "hibongo"

-- | @openhibongo@.
openhibongo           :: (CDrumPitchName repr) => repr DrumPitchName
openhibongo           = drumPitchName "openhibongo"

-- | @mutelobongo@.
mutelobongo           :: (CDrumPitchName repr) => repr DrumPitchName
mutelobongo           = drumPitchName "mutelobongo"

-- | @lobongo@.
lobongo               :: (CDrumPitchName repr) => repr DrumPitchName
lobongo               = drumPitchName "lobongo"

-- | @openlobongo@.
openlobongo           :: (CDrumPitchName repr) => repr DrumPitchName
openlobongo           = drumPitchName "openlobongo"

-- | @mutehiconga@.
mutehiconga           :: (CDrumPitchName repr) => repr DrumPitchName
mutehiconga           = drumPitchName "mutehiconga"

-- | @muteloconga@.
muteloconga           :: (CDrumPitchName repr) => repr DrumPitchName
muteloconga           = drumPitchName "muteloconga"

-- | @openhiconga@.
openhiconga           :: (CDrumPitchName repr) => repr DrumPitchName
openhiconga           = drumPitchName "openhiconga"

-- | @hiconga@.
hiconga               :: (CDrumPitchName repr) => repr DrumPitchName
hiconga               = drumPitchName "hiconga"

-- | @openloconga@.
openloconga           :: (CDrumPitchName repr) => repr DrumPitchName
openloconga           = drumPitchName "openloconga"

-- | @loconga@.
loconga               :: (CDrumPitchName repr) => repr DrumPitchName
loconga               = drumPitchName "loconga"

-- | @hitimbale@.
hitimbale             :: (CDrumPitchName repr) => repr DrumPitchName
hitimbale             = drumPitchName "hitimbale"

-- | @lotimbale@.
lotimbale             :: (CDrumPitchName repr) => repr DrumPitchName
lotimbale             = drumPitchName "lotimbale"

-- | @hiagogo@.
hiagogo               :: (CDrumPitchName repr) => repr DrumPitchName
hiagogo               = drumPitchName "hiagogo"

-- | @loagogo@.
loagogo               :: (CDrumPitchName repr) => repr DrumPitchName
loagogo               = drumPitchName "loagogo"

-- | @cabasa@.
cabasa                :: (CDrumPitchName repr) => repr DrumPitchName
cabasa                = drumPitchName "cabasa"

-- | @maracas@.
maracas               :: (CDrumPitchName repr) => repr DrumPitchName
maracas               = drumPitchName "maracas"

-- | @shortwhistle@.
shortwhistle          :: (CDrumPitchName repr) => repr DrumPitchName
shortwhistle          = drumPitchName "shortwhistle"

-- | @longwhistle@.
longwhistle           :: (CDrumPitchName repr) => repr DrumPitchName
longwhistle           = drumPitchName "longwhistle"

-- | @shortguiro@.
shortguiro            :: (CDrumPitchName repr) => repr DrumPitchName
shortguiro            = drumPitchName "shortguiro"

-- | @longguiro@.
longguiro             :: (CDrumPitchName repr) => repr DrumPitchName
longguiro             = drumPitchName "longguiro"

-- | @guiro@.
guiro                 :: (CDrumPitchName repr) => repr DrumPitchName
guiro                 = drumPitchName "guiro"

-- | @claves@.
claves                :: (CDrumPitchName repr) => repr DrumPitchName
claves                = drumPitchName "claves"

-- | @hiwoodblock@.
hiwoodblock           :: (CDrumPitchName repr) => repr DrumPitchName
hiwoodblock           = drumPitchName "hiwoodblock"

-- | @lowoodblock@.
lowoodblock           :: (CDrumPitchName repr) => repr DrumPitchName
lowoodblock           = drumPitchName "lowoodblock"

-- | @mutecuica@.
mutecuica             :: (CDrumPitchName repr) => repr DrumPitchName
mutecuica             = drumPitchName "mutecuica"

-- | @opencuica@.
opencuica             :: (CDrumPitchName repr) => repr DrumPitchName
opencuica             = drumPitchName "opencuica"

-- | @mutetriangle@.
mutetriangle          :: (CDrumPitchName repr) => repr DrumPitchName
mutetriangle          = drumPitchName "mutetriangle"

-- | @triangle@.
triangle              :: (CDrumPitchName repr) => repr DrumPitchName
triangle              = drumPitchName "triangle"

-- | @opentriangle@.
opentriangle          :: (CDrumPitchName repr) => repr DrumPitchName
opentriangle          = drumPitchName "opentriangle"

-- | @oneup@.
oneup                 :: (CDrumPitchName repr) => repr DrumPitchName
oneup                 = drumPitchName "oneup"

-- | @twoup@.
twoup                 :: (CDrumPitchName repr) => repr DrumPitchName
twoup                 = drumPitchName "twoup"

-- | @threeup@.
threeup               :: (CDrumPitchName repr) => repr DrumPitchName
threeup               = drumPitchName "threeup"

-- | @fourup@.
fourup                :: (CDrumPitchName repr) => repr DrumPitchName
fourup                = drumPitchName "fourup"

-- | @fiveup@.
fiveup                :: (CDrumPitchName repr) => repr DrumPitchName
fiveup                = drumPitchName "fiveup"

-- | @onedown@.
onedown               :: (CDrumPitchName repr) => repr DrumPitchName
onedown               = drumPitchName "onedown"

-- | @twodown@.
twodown               :: (CDrumPitchName repr) => repr DrumPitchName
twodown               = drumPitchName "twodown"

-- | @threedown@.
threedown             :: (CDrumPitchName repr) => repr DrumPitchName
threedown             = drumPitchName "threedown"

-- | @fourdown@.
fourdown              :: (CDrumPitchName repr) => repr DrumPitchName
fourdown              = drumPitchName "fourdown"

-- | @fivedown@.
fivedown              :: (CDrumPitchName repr) => repr DrumPitchName
fivedown              = drumPitchName "fivedown"
   


-- | @bda@ - abbreviated name for 'acousticbassdrum'. 
bda                   :: (CDrumPitchName repr) => repr DrumPitchName
bda                   = drumPitchName "bda"

-- | @bd@ - abbreviated name for 'bassdrum'.
bd                    :: (CDrumPitchName repr) => repr DrumPitchName
bd                    = drumPitchName "bd"

-- | @ssh@ - abbreviated name for 'hisidestick'.
ssh                   :: (CDrumPitchName repr) => repr DrumPitchName
ssh                   = drumPitchName "ssh"

-- | @ss@ - abbreviated name for 'sidestick'.
ss                    :: (CDrumPitchName repr) => repr DrumPitchName
ss                    = drumPitchName "ss"

-- | @ssl@ - abbreviated name for 'losidestick'.
ssl                   :: (CDrumPitchName repr) => repr DrumPitchName
ssl                   = drumPitchName "ssl"

-- | @sna@ - abbreviated name for 'acousticsnare'.
sna                   :: (CDrumPitchName repr) => repr DrumPitchName
sna                   = drumPitchName "sna"

-- | @sn@ - abbreviated name for 'snare'.
sn                    :: (CDrumPitchName repr) => repr DrumPitchName
sn                    = drumPitchName "sn"

-- | @hc@ - abbreviated name for 'handclap'.
hc                    :: (CDrumPitchName repr) => repr DrumPitchName
hc                    = drumPitchName "hc"

-- | @sne@ - abbreviated name for 'electricsnare'.
sne                   :: (CDrumPitchName repr) => repr DrumPitchName
sne                   = drumPitchName "sne"

-- | @tomfl@ - abbreviated name for 'lowfloortom'.
tomfl                 :: (CDrumPitchName repr) => repr DrumPitchName
tomfl                 = drumPitchName "tomfl"

-- | @hhc@ - abbreviated name for 'closedhihat'.
hhc                   :: (CDrumPitchName repr) => repr DrumPitchName
hhc                   = drumPitchName "hhc"

-- | @hh@ - abbreviated name for 'hihat'.
hh                    :: (CDrumPitchName repr) => repr DrumPitchName
hh                    = drumPitchName "hh"

-- | @tomfh@ - abbreviated name for 'highfloortom'.
tomfh                 :: (CDrumPitchName repr) => repr DrumPitchName
tomfh                 = drumPitchName "tomfh"


-- | @hhp@ - abbreviated name for 'pedalhihat'.
hhp                   :: (CDrumPitchName repr) => repr DrumPitchName
hhp                   = drumPitchName "hhp"

-- | @toml@ - abbreviated name for 'lowtom'.
toml                  :: (CDrumPitchName repr) => repr DrumPitchName
toml                  = drumPitchName "toml"

-- | @hho@ - abbreviated name for 'openhihat'.
hho                   :: (CDrumPitchName repr) => repr DrumPitchName
hho                   = drumPitchName "hho"

-- | @hhho@ - abbreviated name for 'halfopenhihat'.
hhho                  :: (CDrumPitchName repr) => repr DrumPitchName
hhho                  = drumPitchName "hhho"

-- | @tomml@ - abbreviated name for 'lowmidtom'.
tomml                 :: (CDrumPitchName repr) => repr DrumPitchName
tomml                 = drumPitchName "tomml"

-- | @tommh@ - abbreviated name for 'himidtom'.
tommh                 :: (CDrumPitchName repr) => repr DrumPitchName
tommh                 = drumPitchName "tommh"

-- | @cymca@ - abbreviated name for 'crashcymbala'.
cymca                 :: (CDrumPitchName repr) => repr DrumPitchName
cymca                 = drumPitchName "cymca"

-- | @cymc@ - abbreviated name for 'crashcymbal'.
cymc                  :: (CDrumPitchName repr) => repr DrumPitchName
cymc                  = drumPitchName "cymc"

-- | @tomh@ - abbreviated name for 'hightom'.
tomh                  :: (CDrumPitchName repr) => repr DrumPitchName
tomh                  = drumPitchName "tomh"

-- | @cymra@ - abbreviated name for 'ridecymbala'.
cymra                 :: (CDrumPitchName repr) => repr DrumPitchName
cymra                 = drumPitchName "cymra"

-- | @cymr@ - abbreviated name for 'ridecymbal'.
cymr                  :: (CDrumPitchName repr) => repr DrumPitchName
cymr                  = drumPitchName "cymr"

-- | @cymch@ - abbreviated name for 'chinesecymbal'.
cymch                 :: (CDrumPitchName repr) => repr DrumPitchName
cymch                 = drumPitchName "cymch"

-- | @rb@ - abbreviated name for 'ridebell'.
rb                    :: (CDrumPitchName repr) => repr DrumPitchName
rb                    = drumPitchName "rb"

-- | @tamb@ - abbreviated name for 'tambourine'.
tamb                  :: (CDrumPitchName repr) => repr DrumPitchName
tamb                  = drumPitchName "tamb"

-- | @cyms@ - abbreviated name for 'splashcymbal'.
cyms                  :: (CDrumPitchName repr) => repr DrumPitchName
cyms                  = drumPitchName "cyms"

-- | @cb@ - abbreviated name for 'cowbell'.
cb                    :: (CDrumPitchName repr) => repr DrumPitchName
cb                    = drumPitchName "cb"

-- | @cymcb@ - abbreviated name for 'crashcymbalb'.
cymcb                 :: (CDrumPitchName repr) => repr DrumPitchName
cymcb                 = drumPitchName "cymcb"

-- | @vibs@ - abbreviated name for 'vibraslap'.
vibs                  :: (CDrumPitchName repr) => repr DrumPitchName
vibs                  = drumPitchName "vibs"

-- | @cymrb@ - abbreviated name for 'ridecymbalb'.
cymrb                 :: (CDrumPitchName repr) => repr DrumPitchName
cymrb                 = drumPitchName "cymrb"

-- | @bohm@ - abbreviated name for 'mutehibongo'.
bohm                  :: (CDrumPitchName repr) => repr DrumPitchName
bohm                  = drumPitchName "bohm"

-- | @boh@ - abbreviated name for 'hibongo'.
boh                   :: (CDrumPitchName repr) => repr DrumPitchName
boh                   = drumPitchName "boh"

-- | @boho@ - abbreviated name for 'openhibongo'.
boho                  :: (CDrumPitchName repr) => repr DrumPitchName
boho                  = drumPitchName "boho"

-- | @bolm@ - abbreviated name for 'mutelobongo'.
bolm                  :: (CDrumPitchName repr) => repr DrumPitchName
bolm                  = drumPitchName "bolm"

-- | @bol@ - abbreviated name for 'lobongo'.
bol                   :: (CDrumPitchName repr) => repr DrumPitchName
bol                   = drumPitchName "bol"

-- | @bolo@ - abbreviated name for 'openlobongo'.
bolo                  :: (CDrumPitchName repr) => repr DrumPitchName
bolo                  = drumPitchName "bolo"

-- | @cghm@ - abbreviated name for 'mutehiconga'.
cghm                  :: (CDrumPitchName repr) => repr DrumPitchName
cghm                  = drumPitchName "cghm"

-- | @cglm@ - abbreviated name for 'muteloconga'.
cglm                  :: (CDrumPitchName repr) => repr DrumPitchName
cglm                  = drumPitchName "cglm"

-- | @cgho@ - abbreviated name for 'openhiconga'.
cgho                  :: (CDrumPitchName repr) => repr DrumPitchName
cgho                  = drumPitchName "cgho"

-- | @cgh@ - abbreviated name for 'hiconga'.
cgh                   :: (CDrumPitchName repr) => repr DrumPitchName
cgh                   = drumPitchName "cgh"

-- | @cglo@ - abbreviated name for 'openloconga'.
cglo                  :: (CDrumPitchName repr) => repr DrumPitchName
cglo                  = drumPitchName "cglo"

-- | @cgl@ - abbreviated name for 'loconga'.
cgl                   :: (CDrumPitchName repr) => repr DrumPitchName
cgl                   = drumPitchName "cgl"

-- | @timh@ - abbreviated name for 'hitimbale'.
timh                  :: (CDrumPitchName repr) => repr DrumPitchName
timh                  = drumPitchName "timh"

-- | @timl@ - abbreviated name for 'lotimbale'.
timl                  :: (CDrumPitchName repr) => repr DrumPitchName
timl                  = drumPitchName "timl"

-- | @agh@ - abbreviated name for 'hiagogo'.
agh                   :: (CDrumPitchName repr) => repr DrumPitchName
agh                   = drumPitchName "agh"

-- | @agl@ - abbreviated name for 'loagogo'.
agl                   :: (CDrumPitchName repr) => repr DrumPitchName
agl                   = drumPitchName "agl"

-- | @cab@ - abbreviated name for 'cabasa'.
cab                   :: (CDrumPitchName repr) => repr DrumPitchName
cab                   = drumPitchName "cab"

-- | @mar@ - abbreviated name for 'maracas'.
mar                   :: (CDrumPitchName repr) => repr DrumPitchName
mar                   = drumPitchName "mar"

-- | @whs@ - abbreviated name for 'shortwhistle'.
whs                   :: (CDrumPitchName repr) => repr DrumPitchName
whs                   = drumPitchName "whs"

-- | @whl@ - abbreviated name for 'longwhistle'.
whl                   :: (CDrumPitchName repr) => repr DrumPitchName
whl                   = drumPitchName "whl"

-- | @guis@ - abbreviated name for 'shortguiro'.
guis                  :: (CDrumPitchName repr) => repr DrumPitchName
guis                  = drumPitchName "guis"

-- | @guil@ - abbreviated name for 'longguiro'.
guil                  :: (CDrumPitchName repr) => repr DrumPitchName
guil                  = drumPitchName "guil"

-- | @gui@ - abbreviated name for 'guiro'.
gui                   :: (CDrumPitchName repr) => repr DrumPitchName
gui                   = drumPitchName "gui"

-- | @cl@ - abbreviated name for 'claves'.
cl                    :: (CDrumPitchName repr) => repr DrumPitchName
cl                    = drumPitchName "cl"

-- | @wbh@ - abbreviated name for 'hiwoodblock'.
wbh                   :: (CDrumPitchName repr) => repr DrumPitchName
wbh                   = drumPitchName "wbh"

-- | @wbl@ - abbreviated name for 'lowoodblock'.
wbl                   :: (CDrumPitchName repr) => repr DrumPitchName
wbl                   = drumPitchName "wbl"

-- | @cuim@ - abbreviated name for 'mutecuica'.
cuim                  :: (CDrumPitchName repr) => repr DrumPitchName
cuim                  = drumPitchName "cuim"

-- | @cuio@ - abbreviated name for 'opencuica'.
cuio                  :: (CDrumPitchName repr) => repr DrumPitchName
cuio                  = drumPitchName "cuio"


-- | @trim@ - abbreviated name for 'mutetriangle'.
trim                  :: (CDrumPitchName repr) => repr DrumPitchName
trim                  = drumPitchName "trim"

-- | @tri@ - abbreviated name for 'triangle'.
tri                   :: (CDrumPitchName repr) => repr DrumPitchName
tri                   = drumPitchName "tri"

-- | @trio@ - abbreviated name for 'opentriangle'.
trio                  :: (CDrumPitchName repr) => repr DrumPitchName
trio                  = drumPitchName "trio"

-- | @tt@ - abbreviated name for 'tamtam'.
tt                    :: (CDrumPitchName repr) => repr DrumPitchName
tt                    = drumPitchName "tt"

-- | @ua@ - abbreviated name for 'oneup'.
ua                    :: (CDrumPitchName repr) => repr DrumPitchName
ua                    = drumPitchName "ua"

-- | @ub@ - abbreviated name for 'twoup'.
ub                    :: (CDrumPitchName repr) => repr DrumPitchName
ub                    = drumPitchName "ub"

-- | @uc@ - abbreviated name for 'threeup'.
uc                    :: (CDrumPitchName repr) => repr DrumPitchName
uc                    = drumPitchName "uc"

-- | @ud@ - abbreviated name for 'fourup'.
ud                    :: (CDrumPitchName repr) => repr DrumPitchName
ud                    = drumPitchName "ud"

-- | @ue@ - abbreviated name for 'fiveup'.
ue                    :: (CDrumPitchName repr) => repr DrumPitchName
ue                    = drumPitchName "ue"

-- | @da@ - abbreviated name for 'onedown'.
da                    :: (CDrumPitchName repr) => repr DrumPitchName
da                    = drumPitchName "da"

-- | @db@ - abbreviated name for 'twodown'.
db                    :: (CDrumPitchName repr) => repr DrumPitchName
db                    = drumPitchName "db"

-- | @dc@ - abbreviated name for 'threedown'.
dc                    :: (CDrumPitchName repr) => repr DrumPitchName
dc                    = drumPitchName "dc"

-- | @dd@ - abbreviated name for 'fourdown'.
dd                    :: (CDrumPitchName repr) => repr DrumPitchName
dd                    = drumPitchName "dd"

-- | @de@ - abbreviated name for 'fivedown'.
de                    :: (CDrumPitchName repr) => repr DrumPitchName
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

in_score :: (CSnocList repr CT_Element,
            ListContext CT_Element a,
            CBlock repr,
            CCmdScore repr) 
        => repr a -> repr CmdScore
in_score e = score (block (elementCtx +++ e))


in_book :: (CSnocList repr CT_Book,
            ListContext CT_Book a,
            CBlock repr,
            CCmdBook repr) 
        => repr a -> repr CmdBook
in_book e = book (block (bookCtx +++ e))


--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)
-- *** Creating titles (10.2.1)

in_header :: (CSnocList repr CT_Header,
              ListContext CT_Header a,
              CBlock repr,
              CCmdHeader repr) 
          => repr a -> repr CmdHeader
in_header e = header (block (headerCtx +++ e))


-- | @dedication@.  
dedication            :: (CHeaderElement repr) => String -> repr HeaderElement
dedication            = headerElement "dedication"

-- | @title@.  
title                 :: (CHeaderElement repr) => String -> repr HeaderElement
title                 = headerElement "title"

-- | @subtitle@.  
subtitle              :: (CHeaderElement repr) => String -> repr HeaderElement
subtitle              = headerElement "subtitle"

-- | @subsubtitle@.  
subsubtitle           :: (CHeaderElement repr) => String -> repr HeaderElement
subsubtitle           = headerElement "subsubtitle"

-- | @poet@.  
poet                  :: (CHeaderElement repr) => String -> repr HeaderElement
poet                  = headerElement "poet"

-- | @composer@.  
composer              :: (CHeaderElement repr) => String -> repr HeaderElement
composer              = headerElement "composer"

-- | @meter@.  
meter                 :: (CHeaderElement repr) => String -> repr HeaderElement
meter                 = headerElement "meter"

-- | @opus@.  
opus                  :: (CHeaderElement repr) => String -> repr HeaderElement
opus                  = headerElement "opus"

-- | @arranger@.  
arranger              :: (CHeaderElement repr) => String -> repr HeaderElement
arranger              = headerElement "arranger"

-- | @instrument@.  
instrument            :: (CHeaderElement repr) => String -> repr HeaderElement
instrument            = headerElement "arranger"

-- | @piece@.  
piece                 :: (CHeaderElement repr) => String -> repr HeaderElement
piece                 = headerElement "piece"

-- breakbefore is representated in the CHeaderElement class
-- because its argument is a bool.

-- | @copyright@.  
copyright             :: (CHeaderElement repr) => String -> repr HeaderElement
copyright             = headerElement "copyright"

-- | @tagline@.  
tagline               :: (CHeaderElement repr) => String -> repr HeaderElement
tagline               = headerElement "tagline"

--------------------------------------------------------------------------------    
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

