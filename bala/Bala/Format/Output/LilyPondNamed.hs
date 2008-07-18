
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Output.OutputLyNamed
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Output combinators for LilyPond. 
-- Similar to Text.XHTML in the Hierarchical Libraries, but with some extra
-- typefulness due to the Ly phantom type.
--
--------------------------------------------------------------------------------

module Bala.Format.Output.LilyPondNamed where

import Bala.Format.Output.OutputBase
import Bala.Format.Output.LilyPondInternals

import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Named elements


-- *** Elements - Normal pitches (6.1.1)

_c, _d, _e, _f, _g , _a, _b :: Ly Pitch
_c      = pitch C
_d      = pitch D
_e      = pitch E
_f      = pitch F
_g      = pitch G
_a      = pitch A
_b      = pitch B

-- *** Elements - Clef (6.4.1)
-- | @treble@.
treble          :: Ly ClefType
treble          = cleftype "treble"

-- | @alto@.
alto            :: Ly ClefType
alto            = cleftype "alto"

-- | @tenor@.
tenor           :: Ly ClefType
tenor           = cleftype "tenor"

-- | @bass@.
bass            :: Ly ClefType
bass            = cleftype "bass"

-- | @french@.
french          :: Ly ClefType
french          = cleftype "french"

-- | @soprano@.
soprano         :: Ly ClefType
soprano         = cleftype "soprano"

-- | @mezzosoprano@.
mezzosoprano    :: Ly ClefType
mezzosoprano    = cleftype "mezzosoprano"

-- | @baritone@.
baritone        :: Ly ClefType
baritone        = cleftype "baritone"

-- | @varbaritone@.
varbaritone     :: Ly ClefType
varbaritone     = cleftype "varbaritone"

-- | @subbass@.
subbass         :: Ly ClefType
subbass         = cleftype "subbass" 

-- | @percussion@.
percussion      :: Ly ClefType
percussion      = cleftype "percussion" 

-- | @tabClef@.
tabClef         :: Ly ClefType
tabClef         = cleftype "tabClef" 


-- *** Elements - Key signature (6.4.2)

-- | @\\major@.
major           :: Ly CmdKeyType
major           = keyType "major"

-- | @\\minor@.
minor           :: Ly CmdKeyType
minor           = keyType "minor"

-- | @\\ionian@.
ionian          :: Ly CmdKeyType
ionian          = keyType "ionian"

-- | @\\locrian@.
locrian         :: Ly CmdKeyType
locrian         = keyType "locrian"

-- | @\\aeolian@.
aeolian         :: Ly CmdKeyType
aeolian         = keyType "aeolian"

-- | @\\mixolydian@.
mixolydian      :: Ly CmdKeyType
mixolydian      = keyType "mixolydian"

-- | @\\lydian@.
lydian          :: Ly CmdKeyType
lydian          = keyType "lydian"

-- | @\\phrygian@.
phrygian        :: Ly CmdKeyType
phrygian        = keyType "phrygian"

-- | @\\dorian@.
dorian          :: Ly CmdKeyType
dorian          = keyType "dorian"


-- *** Elements - Ties (6.5.1)

-- | @\\repeatTie@.
repeatTie       :: Ly CmdTie  
repeatTie       = cmdTie "repeatTie" 

-- | @\\tieUp@.
tieUp           :: Ly CmdTie
tieUp           = cmdTie "tieUp"

-- | @\\tieDown@.
tieDown         :: Ly CmdTie
tieDown         = cmdTie "tieDown"

-- | @\\tieNeutral@.
tieNeutral      :: Ly CmdTie
tieNeutral      = cmdTie "tieNeutral"

-- | @\\tieDotted@.
tieDotted       :: Ly CmdTie
tieDotted       = cmdTie "tieDotted"

-- | @\\tieDashed@.
tieDashed       :: Ly CmdTie
tieDashed       = cmdTie "tieDashed"

-- | @\\tieSolid@.
tieSolid        :: Ly CmdTie
tieSolid        = cmdTie "tieSolid"

-- *** Elements - Slurs (6.5.2)

-- | @\\slurUp@.
slurUp          :: Ly CmdSlur
slurUp          = cmdSlur "slurUp"

-- | @\\slurDown@.
slurDown        :: Ly CmdSlur
slurDown        = cmdSlur "slurDown"

-- | @\\slurNeutral@.
slurNeutral     :: Ly CmdSlur
slurNeutral     = cmdSlur "slurNeutral"

-- | @\\slurDashed@.
slurDashed      :: Ly CmdSlur
slurDashed      = cmdSlur "slurDashed"

-- | @\\slurDotted@.
slurDotted      :: Ly CmdSlur
slurDotted      = cmdSlur "slurDotted"

-- | @\\slurSolid@.
slurSolid       :: Ly CmdSlur
slurSolid       = cmdSlur "slurSolid"


-- *** Elements - Phrasing slurs (6.5.3)

-- | @\\(@.
openPhrasingSlur      :: Ly CmdPhrasingSlur
openPhrasingSlur      = cmdPhrasingSlur "("

-- | @\\)@.
closePhrasingSlur     :: Ly CmdPhrasingSlur
closePhrasingSlur     = cmdPhrasingSlur ")"

-- | @\\phrasingSlurUp@.
phrasingSlurUp        :: Ly CmdPhrasingSlur
phrasingSlurUp        = cmdPhrasingSlur "phrasingSlurUp"

-- | @\\phrasingSlurDown@.
phrasingSlurDown      :: Ly CmdPhrasingSlur
phrasingSlurDown      = cmdPhrasingSlur "phrasingSlurDown"

-- | @\\phrasingSlurNeutral@.
phrasingSlurNeutral   :: Ly CmdPhrasingSlur
phrasingSlurNeutral   = cmdPhrasingSlur "phrasingSlurNeutral"

-- *** Elements - Grace notes (6.5.7)

-- | @\\grace@.
grace         :: Ly a -> Ly CmdGrace
grace         = cmdGrace "grace"

-- | @\\acciaccatura@.
acciaccatura  :: Ly a -> Ly CmdGrace
acciaccatura  = cmdGrace "acciaccatura"

-- | @\\appoggiatura@.
appoggiatura  :: Ly a -> Ly CmdGrace
appoggiatura  = cmdGrace "appoggiatura" 

-- *** Elements - Articulations (6.6.1)

-- | @-^@ - dashHat, aka @marcato@.
dashHat           :: Ly Articulation
dashHat           = articulation "-^"

-- | @-+@ - dashPlus, aka @stopped@.
dashPlus          :: Ly Articulation
dashPlus          = articulation "-+"

-- | @--@ - dashDash, aka @tenuto@.
dashDash          :: Ly Articulation
dashDash          = articulation "--"

-- | @-|@ - dashBar, aka @staccatissimo@.
dashBar           :: Ly Articulation
dashBar           = articulation "-|"

-- | @->@ - dashLarger, aka @accent@.
dashLarger        :: Ly Articulation
dashLarger        = articulation "->"

-- | @-.@ - dashDot, aka @staccato@.
dashDot           :: Ly Articulation
dashDot           = articulation "-."

-- | @-_@ - dashUnderscore, aka @portato@.
dashUnderscore    :: Ly Articulation
dashUnderscore    = articulation "-_"



-- | @\\accent@.
accent                  :: Ly CmdArticulation          
accent                  = cmdArticulation "accent"  

-- | @\\marcato@.
marcato                 :: Ly CmdArticulation
marcato                 = cmdArticulation "marcato" 

-- | @\\staccatissimo@.
staccatissimo           :: Ly CmdArticulation
staccatissimo           = cmdArticulation "staccatissimo" 

-- | @\\espressivo@.
espressivo              :: Ly CmdArticulation  
espressivo              = cmdArticulation "espressivo"  

-- | @\\staccato@.
staccato                :: Ly CmdArticulation
staccato                = cmdArticulation "staccato" 

-- | @\\tenuto@.
tenuto                  :: Ly CmdArticulation
tenuto                  = cmdArticulation "tenuto" 

-- | @\\portato@.
portato                 :: Ly CmdArticulation
portato                 = cmdArticulation "portato"

-- | @\\upbow@.
upbow                   :: Ly CmdArticulation
upbow                   = cmdArticulation "upbow"

-- | @\\downbow@.
downbow                 :: Ly CmdArticulation
downbow                 = cmdArticulation "downbow" 

-- | @\\flageolet@.
flageolet               :: Ly CmdArticulation
flageolet               = cmdArticulation "flageolet" 

-- | @\\thumb@.
thumb                   :: Ly CmdArticulation
thumb                   = cmdArticulation "thumb" 

-- | @\\lheel@.
lheel                   :: Ly CmdArticulation
lheel                   = cmdArticulation "lheel" 

-- | @\\rheel@.
rheel                   :: Ly CmdArticulation
rheel                   = cmdArticulation "rheel" 

-- | @\\ltoe@.
ltoe                    :: Ly CmdArticulation
ltoe                    = cmdArticulation "ltoe" 

-- | @\\rtoe@.
rtoe                    :: Ly CmdArticulation
rtoe                    = cmdArticulation "rtoe" 

-- | @\\open@.
open                    :: Ly CmdArticulation
open                    = cmdArticulation "open"

-- | @\\stopped@.
stopped                 :: Ly CmdArticulation
stopped                 = cmdArticulation "stopped"

-- | @\\turn@.
turn                    :: Ly CmdArticulation
turn                    = cmdArticulation "turn" 

-- | @\\reverseturn@.
reverseturn             :: Ly CmdArticulation
reverseturn             = cmdArticulation "reverseturn"      

-- | @\\trill@.
trill                   :: Ly CmdArticulation
trill                   = cmdArticulation "trill"  

-- | @\\prall@.
prall                   :: Ly CmdArticulation
prall                   = cmdArticulation "prall"  

-- | @\\mordent@.
mordent                 :: Ly CmdArticulation
mordent                 = cmdArticulation "mordent"   

-- | @\\prallprall@.
prallprall              :: Ly CmdArticulation
prallprall              = cmdArticulation "prallprall" 

-- | @\\prallmordent@.
prallmordent            :: Ly CmdArticulation
prallmordent            = cmdArticulation "prallmordent"  

-- | @\\upprall@.
upprall                 :: Ly CmdArticulation
upprall                 = cmdArticulation "upprall"   

-- | @\\downprall@.
downprall               :: Ly CmdArticulation
downprall               = cmdArticulation "downprall" 

-- | @\\upmordent@.
upmordent               :: Ly CmdArticulation
upmordent               = cmdArticulation "upmordent"  

-- | @\\downmordent@.
downmordent             :: Ly CmdArticulation
downmordent             = cmdArticulation "downmordent" 

-- | @\\pralldown@.
pralldown               :: Ly CmdArticulation
pralldown               = cmdArticulation "pralldown"   

-- | @\\prallup@.
prallup                 :: Ly CmdArticulation
prallup                 = cmdArticulation "prallup"  

-- | @\\lineprall@.
lineprall               :: Ly CmdArticulation
lineprall               = cmdArticulation "lineprall" 

-- | @\\signumcongruentiae@.
signumcongruentiae      :: Ly CmdArticulation
signumcongruentiae      = cmdArticulation "signumcongruentiae"  

-- | @\\shortfermata@.
shortfermata            :: Ly CmdArticulation
shortfermata            = cmdArticulation "shortfermata"  

-- | @\\fermata@.
fermata                 :: Ly CmdArticulation
fermata                 = cmdArticulation "fermata" 

-- | @\\longfermata@.
longfermata             :: Ly CmdArticulation
longfermata             = cmdArticulation "longfermata" 

-- | @\\verylongfermata@.
verylongfermata         :: Ly CmdArticulation 
verylongfermata         = cmdArticulation "verylongfermata"    

-- | @\\segno@.
segno                   :: Ly CmdArticulation
segno                   = cmdArticulation "segno" 

-- | @\\coda@.
coda                    :: Ly CmdArticulation
coda                    = cmdArticulation "coda" 

-- | @\\varcoda@.
varcoda                 :: Ly CmdArticulation
varcoda                 = cmdArticulation "varcoda" 

-- *** Elements - Dynamics (6.6.3)
-- use underscore suffix _ so we don't swallow up valuable identifiers in the 
-- namespace

-- | @\\ppppp@.
ppppp_                  :: Ly CmdDynamic
ppppp_                  = cmdDynamic "ppppp" 

-- | @\\pppp@.
pppp_                   :: Ly CmdDynamic
pppp_                   = cmdDynamic "pppp" 

-- | @\\ppp@.
ppp_                    :: Ly CmdDynamic
ppp_                    = cmdDynamic "ppp" 

-- | @\\pp@.
pp_                     :: Ly CmdDynamic
pp_                     = cmdDynamic "pp" 

-- | @\\p@ - renamed piano.
piano                   :: Ly CmdDynamic
piano                   = cmdDynamic "p" 

-- | @\\mp@.
mp_                     :: Ly CmdDynamic
mp_                     = cmdDynamic "mp" 

-- | @\\mf@.
mf_                     :: Ly CmdDynamic
mf_                     = cmdDynamic "mf" 

-- | @\\f@ - renamed forte.
forte                   :: Ly CmdDynamic
forte                   = cmdDynamic "f"

-- | @\\ff@.
ff_                     :: Ly CmdDynamic
ff_                     = cmdDynamic "ff"

-- | @\\fff@.
fff_                    :: Ly CmdDynamic
fff_                    = cmdDynamic "fff"

-- | @\\ffff@.
ffff_                   :: Ly CmdDynamic
ffff_                   = cmdDynamic "ffff"

-- | @\\fp@.
fp_                     :: Ly CmdDynamic
fp_                     = cmdDynamic "fp"

-- | @\\sf@.
sf_                     :: Ly CmdDynamic
sf_                     = cmdDynamic "sf"

-- | @\\sff@.
sff_                    :: Ly CmdDynamic
sff_                    = cmdDynamic "sff"

-- | @\\sp@.
sp_                     :: Ly CmdDynamic
sp_                     = cmdDynamic "sp"

-- | @\\spp@.
spp_                    :: Ly CmdDynamic
spp_                    = cmdDynamic "spp"

-- | @\\sfz@.
sfz_                    :: Ly CmdDynamic
sfz_                    = cmdDynamic "sfz"

-- | @\\rfz@.
rfz_                    :: Ly CmdDynamic
rfz_                    = cmdDynamic "rfz"

-- | @\\<@.
openCrescendo           :: Ly CmdDynamic
openCrescendo           = cmdDynamic "<"

-- | @\\>@.
openDecrescendo         :: Ly CmdDynamic
openDecrescendo         = cmdDynamic ">"
 
-- | @\\!@. 
closeDynamic            :: Ly CmdDynamic 
closeDynamic            = cmdDynamic "!"

-- | @\\cr@ - alias of \\<. 
cr_                     :: Ly CmdDynamic
cr_                     = cmdDynamic "cr" 

-- | @\\decr@ - alias of \\>. 
decr_                   :: Ly CmdDynamic
decr_                   = cmdDynamic "decr"   

-- | @\\dynamicUp@. 
dynamicUp               :: Ly CmdDynamic
dynamicUp               = cmdDynamic "dynamicUp"  

-- | @\\dynamicDown@. 
dynamicDown             :: Ly CmdDynamic
dynamicDown             = cmdDynamic "dynamicDown"  

-- | @\\dynamicNeutral@. 
dynamicNeutral          :: Ly CmdDynamic
dynamicNeutral          = cmdDynamic "dynamicNeutral"  

-- *** Elements - Pedals (7.1.2)  
-- | @\\sustainDown@.
sustainDown       :: Ly CmdPedal
sustainDown       = cmdPedal "sustainDown"  

-- | @\\sustainUp@.
sustainUp         :: Ly CmdPedal 
sustainUp         = cmdPedal "sustainUp"  

-- | @\\unaCorda@.
unaCorda          :: Ly CmdPedal 
unaCorda          = cmdPedal "unaCorda"  

-- | @\\treCorde@.
treCorde          :: Ly CmdPedal 
treCorde          = cmdPedal "treCorde"  

-- | @\\sostenutoDown@.
sostenutoDown     :: Ly CmdPedal 
sostenutoDown     = cmdPedal "sostenutoDown"  

-- | @\\sostenutoUp@.
sostenutoUp       :: Ly CmdPedal 
sostenutoUp       = cmdPedal "sostenutoUp"  


--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

-- | @acousticbassdrum@. 
acousticbassdrum      :: Ly DrumPitchName
acousticbassdrum      = drumPitchName "acousticbassdrum"

-- | @bassdrum@. 
bassdrum              :: Ly DrumPitchName
bassdrum              = drumPitchName "bassdrum"

-- | @hisidestick@. 
hisidestick           :: Ly DrumPitchName
hisidestick           = drumPitchName "hisidestick"

-- | @sidestick@. 
sidestick             :: Ly DrumPitchName
sidestick             = drumPitchName "sidestick"

-- | @losidestick@. 
losidestick           :: Ly DrumPitchName
losidestick           = drumPitchName "losidestick"

-- | @acousticsnare@. 
acousticsnare         :: Ly DrumPitchName
acousticsnare         = drumPitchName "acousticbassdrum"

-- | @snare@. 
snare                 :: Ly DrumPitchName
snare                 = drumPitchName "snare"

-- | @handclap@.
handclap              :: Ly DrumPitchName
handclap              = drumPitchName "handclap"

-- | @electricsnare@.
electricsnare         :: Ly DrumPitchName
electricsnare         = drumPitchName "electricsnare"

-- | @lowfloortom@.
lowfloortom           :: Ly DrumPitchName
lowfloortom           = drumPitchName "lowfloortom"

-- | @closedhihat@.
closedhihat           :: Ly DrumPitchName
closedhihat           = drumPitchName "closedhihat"

-- | @hihat@.
hihat                 :: Ly DrumPitchName
hihat                 = drumPitchName "hihat"

-- | @highfloortom@.
highfloortom          :: Ly DrumPitchName
highfloortom          = drumPitchName "highfloortom"

-- | @pedalhihat@.
pedalhihat            :: Ly DrumPitchName
pedalhihat            = drumPitchName "pedalhihat"

-- | @lowtom@.
lowtom                :: Ly DrumPitchName
lowtom                = drumPitchName "lowtom"

-- | @openhihat@.
openhihat             :: Ly DrumPitchName
openhihat             = drumPitchName "openhihat"

-- | @halfopenhihat@.
halfopenhihat         :: Ly DrumPitchName
halfopenhihat         = drumPitchName "halfopenhihat"

-- | @lowmidtom@.
lowmidtom             :: Ly DrumPitchName
lowmidtom             = drumPitchName "lowmidtom"

-- | @himidtom@.
himidtom              :: Ly DrumPitchName
himidtom              = drumPitchName "himidtom"

-- | @crashcymbala@.
crashcymbala          :: Ly DrumPitchName
crashcymbala          = drumPitchName "crashcymbala"

-- | @crashcymbal@.
crashcymbal           :: Ly DrumPitchName
crashcymbal           = drumPitchName "crashcymbal"

-- | @hightom@.
hightom               :: Ly DrumPitchName
hightom               = drumPitchName "hightom"

-- | @ridecymbala@.

ridecymbala           :: Ly DrumPitchName
ridecymbala           = drumPitchName "ridecymbala"

-- | @ridecymbal@.
ridecymbal            :: Ly DrumPitchName
ridecymbal            = drumPitchName "ridecymbal"

-- | @chinesecymbal@.
chinesecymbal         :: Ly DrumPitchName
chinesecymbal         = drumPitchName "chinesecymbal"

-- | @ridebell@.
ridebell              :: Ly DrumPitchName
ridebell              = drumPitchName "ridebell"

-- | @tambourine@.
tambourine            :: Ly DrumPitchName
tambourine            = drumPitchName "tambourine"

-- | @splashcymbal@.
splashcymbal          :: Ly DrumPitchName
splashcymbal          = drumPitchName "splashcymbal"

-- | @cowbell@.
cowbell               :: Ly DrumPitchName
cowbell               = drumPitchName "cowbell"

-- | @crashcymbalb@.
crashcymbalb          :: Ly DrumPitchName
crashcymbalb          = drumPitchName "crashcymbalb"

-- | @vibraslap@.
vibraslap             :: Ly DrumPitchName
vibraslap             = drumPitchName "vibraslap"

-- | @ridecymbalb@.
ridecymbalb           :: Ly DrumPitchName
ridecymbalb           = drumPitchName "ridecymbalb"

-- | @mutehibongo@.
mutehibongo           :: Ly DrumPitchName
mutehibongo           = drumPitchName "mutehibongo"

-- | @hibongo@.
hibongo               :: Ly DrumPitchName
hibongo               = drumPitchName "hibongo"

-- | @openhibongo@.
openhibongo           :: Ly DrumPitchName
openhibongo           = drumPitchName "openhibongo"

-- | @mutelobongo@.
mutelobongo           :: Ly DrumPitchName
mutelobongo           = drumPitchName "mutelobongo"

-- | @lobongo@.
lobongo               :: Ly DrumPitchName
lobongo               = drumPitchName "lobongo"

-- | @openlobongo@.
openlobongo           :: Ly DrumPitchName
openlobongo           = drumPitchName "openlobongo"

-- | @mutehiconga@.
mutehiconga           :: Ly DrumPitchName
mutehiconga           = drumPitchName "mutehiconga"

-- | @muteloconga@.
muteloconga           :: Ly DrumPitchName
muteloconga           = drumPitchName "muteloconga"

-- | @openhiconga@.
openhiconga           :: Ly DrumPitchName
openhiconga           = drumPitchName "openhiconga"

-- | @hiconga@.
hiconga               :: Ly DrumPitchName
hiconga               = drumPitchName "hiconga"

-- | @openloconga@.
openloconga           :: Ly DrumPitchName
openloconga           = drumPitchName "openloconga"

-- | @loconga@.
loconga               :: Ly DrumPitchName
loconga               = drumPitchName "loconga"

-- | @hitimbale@.
hitimbale             :: Ly DrumPitchName
hitimbale             = drumPitchName "hitimbale"

-- | @lotimbale@.
lotimbale             :: Ly DrumPitchName
lotimbale             = drumPitchName "lotimbale"

-- | @hiagogo@.
hiagogo               :: Ly DrumPitchName
hiagogo               = drumPitchName "hiagogo"

-- | @loagogo@.
loagogo               :: Ly DrumPitchName
loagogo               = drumPitchName "loagogo"

-- | @cabasa@.
cabasa                :: Ly DrumPitchName
cabasa                = drumPitchName "cabasa"

-- | @maracas@.
maracas               :: Ly DrumPitchName
maracas               = drumPitchName "maracas"

-- | @shortwhistle@.
shortwhistle          :: Ly DrumPitchName
shortwhistle          = drumPitchName "shortwhistle"

-- | @longwhistle@.
longwhistle           :: Ly DrumPitchName
longwhistle           = drumPitchName "longwhistle"

-- | @shortguiro@.
shortguiro            :: Ly DrumPitchName
shortguiro            = drumPitchName "shortguiro"

-- | @longguiro@.
longguiro             :: Ly DrumPitchName
longguiro             = drumPitchName "longguiro"

-- | @guiro@.
guiro                 :: Ly DrumPitchName
guiro                 = drumPitchName "guiro"

-- | @claves@.
claves                :: Ly DrumPitchName
claves                = drumPitchName "claves"

-- | @hiwoodblock@.
hiwoodblock           :: Ly DrumPitchName
hiwoodblock           = drumPitchName "hiwoodblock"

-- | @lowoodblock@.
lowoodblock           :: Ly DrumPitchName
lowoodblock           = drumPitchName "lowoodblock"

-- | @mutecuica@.
mutecuica             :: Ly DrumPitchName
mutecuica             = drumPitchName "mutecuica"

-- | @opencuica@.
opencuica             :: Ly DrumPitchName
opencuica             = drumPitchName "opencuica"

-- | @mutetriangle@.
mutetriangle          :: Ly DrumPitchName
mutetriangle          = drumPitchName "mutetriangle"

-- | @triangle@.
triangle              :: Ly DrumPitchName
triangle              = drumPitchName "triangle"

-- | @opentriangle@.
opentriangle          :: Ly DrumPitchName
opentriangle          = drumPitchName "opentriangle"

-- | @oneup@.
oneup                 :: Ly DrumPitchName
oneup                 = drumPitchName "oneup"

-- | @twoup@.
twoup                 :: Ly DrumPitchName
twoup                 = drumPitchName "twoup"

-- | @threeup@.
threeup               :: Ly DrumPitchName
threeup               = drumPitchName "threeup"

-- | @fourup@.
fourup                :: Ly DrumPitchName
fourup                = drumPitchName "fourup"

-- | @fiveup@.
fiveup                :: Ly DrumPitchName
fiveup                = drumPitchName "fiveup"

-- | @onedown@.
onedown               :: Ly DrumPitchName
onedown               = drumPitchName "onedown"

-- | @twodown@.
twodown               :: Ly DrumPitchName
twodown               = drumPitchName "twodown"

-- | @threedown@.
threedown             :: Ly DrumPitchName
threedown             = drumPitchName "threedown"

-- | @fourdown@.
fourdown              :: Ly DrumPitchName
fourdown              = drumPitchName "fourdown"

-- | @fivedown@.
fivedown              :: Ly DrumPitchName
fivedown              = drumPitchName "fivedown"
   


-- | @bda@ - abbreviated name for 'acousticbassdrum'. 
bda                   :: Ly DrumPitchName
bda                   = drumPitchName "bda"

-- | @bd@ - abbreviated name for 'bassdrum'.
bd                    :: Ly DrumPitchName
bd                    = drumPitchName "bd"

-- | @ssh@ - abbreviated name for 'hisidestick'.
ssh                   :: Ly DrumPitchName
ssh                   = drumPitchName "ssh"

-- | @ss@ - abbreviated name for 'sidestick'.
ss                    :: Ly DrumPitchName
ss                    = drumPitchName "ss"

-- | @ssl@ - abbreviated name for 'losidestick'.
ssl                   :: Ly DrumPitchName
ssl                   = drumPitchName "ssl"

-- | @sna@ - abbreviated name for 'acousticsnare'.
sna                   :: Ly DrumPitchName
sna                   = drumPitchName "sna"

-- | @sn@ - abbreviated name for 'snare'.
sn                    :: Ly DrumPitchName
sn                    = drumPitchName "sn"

-- | @hc@ - abbreviated name for 'handclap'.
hc                    :: Ly DrumPitchName
hc                    = drumPitchName "hc"

-- | @sne@ - abbreviated name for 'electricsnare'.
sne                   :: Ly DrumPitchName
sne                   = drumPitchName "sne"

-- | @tomfl@ - abbreviated name for 'lowfloortom'.
tomfl                 :: Ly DrumPitchName
tomfl                 = drumPitchName "tomfl"

-- | @hhc@ - abbreviated name for 'closedhihat'.
hhc                   :: Ly DrumPitchName
hhc                   = drumPitchName "hhc"

-- | @hh@ - abbreviated name for 'hihat'.
hh                    :: Ly DrumPitchName
hh                    = drumPitchName "hh"

-- | @tomfh@ - abbreviated name for 'highfloortom'.
tomfh                 :: Ly DrumPitchName
tomfh                 = drumPitchName "tomfh"


-- | @hhp@ - abbreviated name for 'pedalhihat'.
hhp                   :: Ly DrumPitchName
hhp                   = drumPitchName "hhp"

-- | @toml@ - abbreviated name for 'lowtom'.
toml                  :: Ly DrumPitchName
toml                  = drumPitchName "toml"

-- | @hho@ - abbreviated name for 'openhihat'.
hho                   :: Ly DrumPitchName
hho                   = drumPitchName "hho"

-- | @hhho@ - abbreviated name for 'halfopenhihat'.
hhho                  :: Ly DrumPitchName
hhho                  = drumPitchName "hhho"

-- | @tomml@ - abbreviated name for 'lowmidtom'.
tomml                 :: Ly DrumPitchName
tomml                 = drumPitchName "tomml"

-- | @tommh@ - abbreviated name for 'himidtom'.
tommh                 :: Ly DrumPitchName
tommh                 = drumPitchName "tommh"

-- | @cymca@ - abbreviated name for 'crashcymbala'.
cymca                 :: Ly DrumPitchName
cymca                 = drumPitchName "cymca"

-- | @cymc@ - abbreviated name for 'crashcymbal'.
cymc                  :: Ly DrumPitchName
cymc                  = drumPitchName "cymc"

-- | @tomh@ - abbreviated name for 'hightom'.
tomh                  :: Ly DrumPitchName
tomh                  = drumPitchName "tomh"

-- | @cymra@ - abbreviated name for 'ridecymbala'.
cymra                 :: Ly DrumPitchName
cymra                 = drumPitchName "cymra"

-- | @cymr@ - abbreviated name for 'ridecymbal'.
cymr                  :: Ly DrumPitchName
cymr                  = drumPitchName "cymr"

-- | @cymch@ - abbreviated name for 'chinesecymbal'.
cymch                 :: Ly DrumPitchName
cymch                 = drumPitchName "cymch"

-- | @rb@ - abbreviated name for 'ridebell'.
rb                    :: Ly DrumPitchName
rb                    = drumPitchName "rb"

-- | @tamb@ - abbreviated name for 'tambourine'.
tamb                  :: Ly DrumPitchName
tamb                  = drumPitchName "tamb"

-- | @cyms@ - abbreviated name for 'splashcymbal'.
cyms                  :: Ly DrumPitchName
cyms                  = drumPitchName "cyms"

-- | @cb@ - abbreviated name for 'cowbell'.
cb                    :: Ly DrumPitchName
cb                    = drumPitchName "cb"

-- | @cymcb@ - abbreviated name for 'crashcymbalb'.
cymcb                 :: Ly DrumPitchName
cymcb                 = drumPitchName "cymcb"

-- | @vibs@ - abbreviated name for 'vibraslap'.
vibs                  :: Ly DrumPitchName
vibs                  = drumPitchName "vibs"

-- | @cymrb@ - abbreviated name for 'ridecymbalb'.
cymrb                 :: Ly DrumPitchName
cymrb                 = drumPitchName "cymrb"

-- | @bohm@ - abbreviated name for 'mutehibongo'.
bohm                  :: Ly DrumPitchName
bohm                  = drumPitchName "bohm"

-- | @boh@ - abbreviated name for 'hibongo'.
boh                   :: Ly DrumPitchName
boh                   = drumPitchName "boh"

-- | @boho@ - abbreviated name for 'openhibongo'.
boho                  :: Ly DrumPitchName
boho                  = drumPitchName "boho"

-- | @bolm@ - abbreviated name for 'mutelobongo'.
bolm                  :: Ly DrumPitchName
bolm                  = drumPitchName "bolm"

-- | @bol@ - abbreviated name for 'lobongo'.
bol                   :: Ly DrumPitchName
bol                   = drumPitchName "bol"

-- | @bolo@ - abbreviated name for 'openlobongo'.
bolo                  :: Ly DrumPitchName
bolo                  = drumPitchName "bolo"

-- | @cghm@ - abbreviated name for 'mutehiconga'.
cghm                  :: Ly DrumPitchName
cghm                  = drumPitchName "cghm"

-- | @cglm@ - abbreviated name for 'muteloconga'.
cglm                  :: Ly DrumPitchName
cglm                  = drumPitchName "cglm"

-- | @cgho@ - abbreviated name for 'openhiconga'.
cgho                  :: Ly DrumPitchName
cgho                  = drumPitchName "cgho"

-- | @cgh@ - abbreviated name for 'hiconga'.
cgh                   :: Ly DrumPitchName
cgh                   = drumPitchName "cgh"

-- | @cglo@ - abbreviated name for 'openloconga'.
cglo                  :: Ly DrumPitchName
cglo                  = drumPitchName "cglo"

-- | @cgl@ - abbreviated name for 'loconga'.
cgl                   :: Ly DrumPitchName
cgl                   = drumPitchName "cgl"

-- | @timh@ - abbreviated name for 'hitimbale'.
timh                  :: Ly DrumPitchName
timh                  = drumPitchName "timh"

-- | @timl@ - abbreviated name for 'lotimbale'.
timl                  :: Ly DrumPitchName
timl                  = drumPitchName "timl"

-- | @agh@ - abbreviated name for 'hiagogo'.
agh                   :: Ly DrumPitchName
agh                   = drumPitchName "agh"

-- | @agl@ - abbreviated name for 'loagogo'.
agl                   :: Ly DrumPitchName
agl                   = drumPitchName "agl"

-- | @cab@ - abbreviated name for 'cabasa'.
cab                   :: Ly DrumPitchName
cab                   = drumPitchName "cab"

-- | @mar@ - abbreviated name for 'maracas'.
mar                   :: Ly DrumPitchName
mar                   = drumPitchName "mar"

-- | @whs@ - abbreviated name for 'shortwhistle'.
whs                   :: Ly DrumPitchName
whs                   = drumPitchName "whs"

-- | @whl@ - abbreviated name for 'longwhistle'.
whl                   :: Ly DrumPitchName
whl                   = drumPitchName "whl"

-- | @guis@ - abbreviated name for 'shortguiro'.
guis                  :: Ly DrumPitchName
guis                  = drumPitchName "guis"

-- | @guil@ - abbreviated name for 'longguiro'.
guil                  :: Ly DrumPitchName
guil                  = drumPitchName "guil"

-- | @gui@ - abbreviated name for 'guiro'.
gui                   :: Ly DrumPitchName
gui                   = drumPitchName "gui"

-- | @cl@ - abbreviated name for 'claves'.
cl                    :: Ly DrumPitchName
cl                    = drumPitchName "cl"

-- | @wbh@ - abbreviated name for 'hiwoodblock'.
wbh                   :: Ly DrumPitchName
wbh                   = drumPitchName "wbh"

-- | @wbl@ - abbreviated name for 'lowoodblock'.
wbl                   :: Ly DrumPitchName
wbl                   = drumPitchName "wbl"

-- | @cuim@ - abbreviated name for 'mutecuica'.
cuim                  :: Ly DrumPitchName
cuim                  = drumPitchName "cuim"

-- | @cuio@ - abbreviated name for 'opencuica'.
cuio                  :: Ly DrumPitchName
cuio                  = drumPitchName "cuio"


-- | @trim@ - abbreviated name for 'mutetriangle'.
trim                  :: Ly DrumPitchName
trim                  = drumPitchName "trim"

-- | @tri@ - abbreviated name for 'triangle'.
tri                   :: Ly DrumPitchName
tri                   = drumPitchName "tri"

-- | @trio@ - abbreviated name for 'opentriangle'.
trio                  :: Ly DrumPitchName
trio                  = drumPitchName "trio"

-- | @tt@ - abbreviated name for 'tamtam'.
tt                    :: Ly DrumPitchName
tt                    = drumPitchName "tt"

-- | @ua@ - abbreviated name for 'oneup'.
ua                    :: Ly DrumPitchName
ua                    = drumPitchName "ua"

-- | @ub@ - abbreviated name for 'twoup'.
ub                    :: Ly DrumPitchName
ub                    = drumPitchName "ub"

-- | @uc@ - abbreviated name for 'threeup'.
uc                    :: Ly DrumPitchName
uc                    = drumPitchName "uc"

-- | @ud@ - abbreviated name for 'fourup'.
ud                    :: Ly DrumPitchName
ud                    = drumPitchName "ud"

-- | @ue@ - abbreviated name for 'fiveup'.
ue                    :: Ly DrumPitchName
ue                    = drumPitchName "ue"

-- | @da@ - abbreviated name for 'onedown'.
da                    :: Ly DrumPitchName
da                    = drumPitchName "da"

-- | @db@ - abbreviated name for 'twodown'.
db                    :: Ly DrumPitchName
db                    = drumPitchName "db"

-- | @dc@ - abbreviated name for 'threedown'.
dc                    :: Ly DrumPitchName
dc                    = drumPitchName "dc"

-- | @dd@ - abbreviated name for 'fourdown'.
dd                    :: Ly DrumPitchName
dd                    = drumPitchName "dd"

-- | @de@ - abbreviated name for 'fivedown'.
de                    :: Ly DrumPitchName
de                    = drumPitchName "de"


--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)
-- *** Creating titles (10.2.1)

-- | @dedication@.  
dedication            :: String -> Ly HeaderElement
dedication            = headerElement "dedication"

-- | @title@.  
title                 :: String -> Ly HeaderElement
title                 = headerElement "title"

-- | @subtitle@.  
subtitle              :: String -> Ly HeaderElement
subtitle              = headerElement "subtitle"

-- | @subsubtitle@.  
subsubtitle           :: String -> Ly HeaderElement
subsubtitle           = headerElement "subsubtitle"

-- | @poet@.  
poet                  :: String -> Ly HeaderElement
poet                  = headerElement "poet"

-- | @composer@.  
composer              :: String -> Ly HeaderElement
composer              = headerElement "composer"

-- | @meter@.  
meter                 :: String -> Ly HeaderElement
meter                 = headerElement "meter"

-- | @opus@.  
opus                  :: String -> Ly HeaderElement
opus                  = headerElement "opus"

-- | @arranger@.  
arranger              :: String -> Ly HeaderElement
arranger              = headerElement "arranger"

-- | @instrument@.  
instrument            :: String -> Ly HeaderElement
instrument            = headerElement "arranger"

-- | @piece@.  
piece                 :: String -> Ly HeaderElement
piece                 = headerElement "piece"


-- | @copyright@.  
copyright             :: String -> Ly HeaderElement
copyright             = headerElement "copyright"

-- | @tagline@.  
tagline               :: String -> Ly HeaderElement
tagline               = headerElement "tagline"

-- | @breakbefore@.
breakbefore             :: Bool -> Ly HeaderElement
breakbefore True        = equation "breakbefore" (lyLit $ text "##t")
breakbefore False       = equation "breakbefore" (lyLit $ text "##f")