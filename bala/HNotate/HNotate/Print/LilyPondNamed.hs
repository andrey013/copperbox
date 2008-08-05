
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Print.OutputLyNamed
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

module HNotate.Print.LilyPondNamed where

import HNotate.Print.Base
import HNotate.Print.LilyPondInternals

import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Named elements


-- *** Elements - Normal pitches (6.1.1)

_c, _d, _e, _f, _g , _a, _b :: LyPitch
_c      = pitch C
_d      = pitch D
_e      = pitch E
_f      = pitch F
_g      = pitch G
_a      = pitch A
_b      = pitch B

-- *** Elements - Clef (6.4.1)
-- | @treble@.
treble              :: LyClefType
treble              = cleftype "treble"

-- | @alto@.
alto                :: LyClefType
alto                = cleftype "alto"

-- | @tenor@.
tenor               :: LyClefType
tenor               = cleftype "tenor"

-- | @bass@.
bass                :: LyClefType
bass                = cleftype "bass"

-- | @french@.
french              :: LyClefType
french              = cleftype "french"

-- | @soprano@.
soprano             :: LyClefType
soprano             = cleftype "soprano"

-- | @mezzosoprano@.
mezzosoprano        :: LyClefType
mezzosoprano        = cleftype "mezzosoprano"

-- | @baritone@.
baritone            :: LyClefType
baritone            = cleftype "baritone"

-- | @varbaritone@.
varbaritone         :: LyClefType
varbaritone         = cleftype "varbaritone"

-- | @subbass@.
subbass             :: LyClefType
subbass             = cleftype "subbass"

-- | @percussion@.
percussion          :: LyClefType
percussion          = cleftype "percussion"

-- | @tabClef@.
tabClef             :: LyClefType
tabClef             = cleftype "tabClef"


-- *** Elements - Key signature (6.4.2)

-- | @\\major@.
major               :: LyCmdKeyType
major               = keyType "major"

-- | @\\minor@.
minor               :: LyCmdKeyType
minor               = keyType "minor"

-- | @\\ionian@.
ionian              :: LyCmdKeyType
ionian              = keyType "ionian"

-- | @\\locrian@.
locrian             :: LyCmdKeyType
locrian             = keyType "locrian"

-- | @\\aeolian@.
aeolian             :: LyCmdKeyType
aeolian             = keyType "aeolian"

-- | @\\mixolydian@.
mixolydian          :: LyCmdKeyType
mixolydian          = keyType "mixolydian"

-- | @\\lydian@.
lydian              :: LyCmdKeyType
lydian              = keyType "lydian"

-- | @\\phrygian@.
phrygian            :: LyCmdKeyType
phrygian            = keyType "phrygian"

-- | @\\dorian@.
dorian              :: LyCmdKeyType
dorian              = keyType "dorian"


-- *** Elements - Ties (6.5.1)

-- | @\\repeatTie@.
repeatTie           :: LyCmdTie
repeatTie           = cmdTie "repeatTie"

-- | @\\tieUp@.
tieUp               :: LyCmdTie
tieUp               = cmdTie "tieUp"

-- | @\\tieDown@.
tieDown             :: LyCmdTie
tieDown             = cmdTie "tieDown"

-- | @\\tieNeutral@.
tieNeutral          :: LyCmdTie
tieNeutral          = cmdTie "tieNeutral"

-- | @\\tieDotted@.
tieDotted           :: LyCmdTie
tieDotted           = cmdTie "tieDotted"

-- | @\\tieDashed@.
tieDashed           :: LyCmdTie
tieDashed           = cmdTie "tieDashed"

-- | @\\tieSolid@.
tieSolid            :: LyCmdTie
tieSolid            = cmdTie "tieSolid"

-- *** Elements - Slurs (6.5.2)

-- | @\\slurUp@.
slurUp              :: LyCmdSlur
slurUp              = cmdSlur "slurUp"

-- | @\\slurDown@.
slurDown            :: LyCmdSlur
slurDown            = cmdSlur "slurDown"

-- | @\\slurNeutral@.
slurNeutral         :: LyCmdSlur
slurNeutral         = cmdSlur "slurNeutral"

-- | @\\slurDashed@.
slurDashed          :: LyCmdSlur
slurDashed          = cmdSlur "slurDashed"

-- | @\\slurDotted@.
slurDotted          :: LyCmdSlur
slurDotted          = cmdSlur "slurDotted"

-- | @\\slurSolid@.
slurSolid           :: LyCmdSlur
slurSolid           = cmdSlur "slurSolid"


-- *** Elements - Phrasing slurs (6.5.3)

-- | @\\(@.
openPhrasingSlur    :: LyCmdPhrasingSlur
openPhrasingSlur    = cmdPhrasingSlur "("

-- | @\\)@.
closePhrasingSlur   :: LyCmdPhrasingSlur
closePhrasingSlur   = cmdPhrasingSlur ")"

-- | @\\phrasingSlurUp@.
phrasingSlurUp      :: LyCmdPhrasingSlur
phrasingSlurUp      = cmdPhrasingSlur "phrasingSlurUp"

-- | @\\phrasingSlurDown@.
phrasingSlurDown    :: LyCmdPhrasingSlur
phrasingSlurDown    = cmdPhrasingSlur "phrasingSlurDown"

-- | @\\phrasingSlurNeutral@.
phrasingSlurNeutral :: LyCmdPhrasingSlur
phrasingSlurNeutral = cmdPhrasingSlur "phrasingSlurNeutral"

-- *** Elements - Grace notes (6.5.7)

-- | @\\grace@.
grace               :: Ly a -> LyCmdGrace
grace               = cmdGrace "grace"

-- | @\\acciaccatura@.
acciaccatura        :: Ly a -> LyCmdGrace
acciaccatura        = cmdGrace "acciaccatura"

-- | @\\appoggiatura@.
appoggiatura        :: Ly a -> LyCmdGrace
appoggiatura        = cmdGrace "appoggiatura"

-- *** Elements - Articulations (6.6.1)

-- | @-^@ - dashHat, aka @marcato@.
dashHat             :: LyArticulation
dashHat             = articulation "-^"

-- | @-+@ - dashPlus, aka @stopped@.
dashPlus            :: LyArticulation
dashPlus            = articulation "-+"

-- | @--@ - dashDash, aka @tenuto@.
dashDash            :: LyArticulation
dashDash            = articulation "--"

-- | @-|@ - dashBar, aka @staccatissimo@.
dashBar             :: LyArticulation
dashBar             = articulation "-|"

-- | @->@ - dashLarger, aka @accent@.
dashLarger          :: LyArticulation
dashLarger          = articulation "->"

-- | @-.@ - dashDot, aka @staccato@.
dashDot             :: LyArticulation
dashDot             = articulation "-."

-- | @-_@ - dashUnderscore, aka @portato@.
dashUnderscore      :: LyArticulation
dashUnderscore      = articulation "-_"



-- | @\\accent@.
accent              :: LyCmdArticulation
accent              = cmdArticulation "accent"

-- | @\\marcato@.
marcato             :: LyCmdArticulation
marcato             = cmdArticulation "marcato"

-- | @\\staccatissimo@.
staccatissimo       :: LyCmdArticulation
staccatissimo       = cmdArticulation "staccatissimo"

-- | @\\espressivo@.
espressivo          :: LyCmdArticulation
espressivo          = cmdArticulation "espressivo"

-- | @\\staccato@.
staccato            :: LyCmdArticulation
staccato            = cmdArticulation "staccato"

-- | @\\tenuto@.
tenuto              :: LyCmdArticulation
tenuto              = cmdArticulation "tenuto"

-- | @\\portato@.
portato             :: LyCmdArticulation
portato             = cmdArticulation "portato"

-- | @\\upbow@.
upbow               :: LyCmdArticulation
upbow               = cmdArticulation "upbow"

-- | @\\downbow@.
downbow             :: LyCmdArticulation
downbow             = cmdArticulation "downbow"

-- | @\\flageolet@.
flageolet           :: LyCmdArticulation
flageolet           = cmdArticulation "flageolet"

-- | @\\thumb@.
thumb               :: LyCmdArticulation
thumb               = cmdArticulation "thumb"

-- | @\\lheel@.
lheel               :: LyCmdArticulation
lheel               = cmdArticulation "lheel"

-- | @\\rheel@.
rheel               :: LyCmdArticulation
rheel               = cmdArticulation "rheel"

-- | @\\ltoe@.
ltoe                :: LyCmdArticulation
ltoe                = cmdArticulation "ltoe"

-- | @\\rtoe@.
rtoe                :: LyCmdArticulation
rtoe                = cmdArticulation "rtoe"

-- | @\\open@.
open                :: LyCmdArticulation
open                = cmdArticulation "open"

-- | @\\stopped@.
stopped             :: LyCmdArticulation
stopped             = cmdArticulation "stopped"

-- | @\\turn@.
turn                :: LyCmdArticulation
turn                = cmdArticulation "turn"

-- | @\\reverseturn@.
reverseturn         :: LyCmdArticulation
reverseturn         = cmdArticulation "reverseturn"

-- | @\\trill@.
trill               :: LyCmdArticulation
trill               = cmdArticulation "trill"

-- | @\\prall@.
prall               :: LyCmdArticulation
prall               = cmdArticulation "prall"

-- | @\\mordent@.
mordent             :: LyCmdArticulation
mordent             = cmdArticulation "mordent"

-- | @\\prallprall@.
prallprall          :: LyCmdArticulation
prallprall          = cmdArticulation "prallprall"

-- | @\\prallmordent@.
prallmordent        :: LyCmdArticulation
prallmordent        = cmdArticulation "prallmordent"

-- | @\\upprall@.
upprall             :: LyCmdArticulation
upprall             = cmdArticulation "upprall"

-- | @\\downprall@.
downprall           :: LyCmdArticulation
downprall           = cmdArticulation "downprall"

-- | @\\upmordent@.
upmordent           :: LyCmdArticulation
upmordent           = cmdArticulation "upmordent"

-- | @\\downmordent@.
downmordent         :: LyCmdArticulation
downmordent         = cmdArticulation "downmordent"

-- | @\\pralldown@.
pralldown           :: LyCmdArticulation
pralldown           = cmdArticulation "pralldown"

-- | @\\prallup@.
prallup             :: LyCmdArticulation
prallup             = cmdArticulation "prallup"

-- | @\\lineprall@.
lineprall           :: LyCmdArticulation
lineprall           = cmdArticulation "lineprall"

-- | @\\signumcongruentiae@.
signumcongruentiae  :: LyCmdArticulation
signumcongruentiae  = cmdArticulation "signumcongruentiae"

-- | @\\shortfermata@.
shortfermata        :: LyCmdArticulation
shortfermata        = cmdArticulation "shortfermata"

-- | @\\fermata@.
fermata             :: LyCmdArticulation
fermata             = cmdArticulation "fermata"

-- | @\\longfermata@.
longfermata         :: LyCmdArticulation
longfermata         = cmdArticulation "longfermata"

-- | @\\verylongfermata@.
verylongfermata     :: LyCmdArticulation
verylongfermata     = cmdArticulation "verylongfermata"

-- | @\\segno@.
segno               :: LyCmdArticulation
segno               = cmdArticulation "segno"

-- | @\\coda@.
coda                :: LyCmdArticulation
coda                = cmdArticulation "coda"

-- | @\\varcoda@.
varcoda             :: LyCmdArticulation
varcoda             = cmdArticulation "varcoda"

-- *** Elements - Dynamics (6.6.3)
-- use underscore suffix _ so we don't swallow up valuable identifiers in the
-- namespace

-- | @\\ppppp@.
ppppp_              :: LyCmdDynamic
ppppp_              = cmdDynamic "ppppp"

-- | @\\pppp@.
pppp_               :: LyCmdDynamic
pppp_               = cmdDynamic "pppp"

-- | @\\ppp@.
ppp_                :: LyCmdDynamic
ppp_                = cmdDynamic "ppp"

-- | @\\pp@.
pp_                 :: LyCmdDynamic
pp_                 = cmdDynamic "pp"

-- | @\\p@ - renamed piano.
piano               :: LyCmdDynamic
piano               = cmdDynamic "p"

-- | @\\mp@.
mp_                 :: LyCmdDynamic
mp_                 = cmdDynamic "mp"

-- | @\\mf@.
mf_                 :: LyCmdDynamic
mf_                 = cmdDynamic "mf"

-- | @\\f@ - renamed forte.
forte               :: LyCmdDynamic
forte               = cmdDynamic "f"

-- | @\\ff@.
ff_                 :: LyCmdDynamic
ff_                 = cmdDynamic "ff"

-- | @\\fff@.
fff_                :: LyCmdDynamic
fff_                = cmdDynamic "fff"

-- | @\\ffff@.
ffff_               :: LyCmdDynamic
ffff_               = cmdDynamic "ffff"

-- | @\\fp@.
fp_                 :: LyCmdDynamic
fp_                 = cmdDynamic "fp"

-- | @\\sf@.
sf_                 :: LyCmdDynamic
sf_                 = cmdDynamic "sf"

-- | @\\sff@.
sff_                :: LyCmdDynamic
sff_                = cmdDynamic "sff"

-- | @\\sp@.
sp_                 :: LyCmdDynamic
sp_                 = cmdDynamic "sp"

-- | @\\spp@.
spp_                :: LyCmdDynamic
spp_                = cmdDynamic "spp"

-- | @\\sfz@.
sfz_                :: LyCmdDynamic
sfz_                = cmdDynamic "sfz"

-- | @\\rfz@.
rfz_                :: LyCmdDynamic
rfz_                = cmdDynamic "rfz"

-- | @\\<@.
openCrescendo       :: LyCmdDynamic
openCrescendo       = cmdDynamic "<"

-- | @\\>@.
openDecrescendo     :: LyCmdDynamic
openDecrescendo     = cmdDynamic ">"

-- | @\\!@.
closeDynamic        :: LyCmdDynamic
closeDynamic        = cmdDynamic "!"

-- | @\\cr@ - alias of \\<.
cr_                 :: LyCmdDynamic
cr_                 = cmdDynamic "cr"

-- | @\\decr@ - alias of \\>.
decr_               :: LyCmdDynamic
decr_               = cmdDynamic "decr"

-- | @\\dynamicUp@.
dynamicUp           :: LyCmdDynamic
dynamicUp           = cmdDynamic "dynamicUp"

-- | @\\dynamicDown@.
dynamicDown         :: LyCmdDynamic
dynamicDown         = cmdDynamic "dynamicDown"

-- | @\\dynamicNeutral@.
dynamicNeutral      :: LyCmdDynamic
dynamicNeutral      = cmdDynamic "dynamicNeutral"

-- *** Elements - Pedals (7.1.2)
-- | @\\sustainDown@.
sustainDown         :: LyCmdPedal
sustainDown         = cmdPedal "sustainDown"

-- | @\\sustainUp@.
sustainUp           :: LyCmdPedal
sustainUp           = cmdPedal "sustainUp"

-- | @\\unaCorda@.
unaCorda            :: LyCmdPedal
unaCorda            = cmdPedal "unaCorda"

-- | @\\treCorde@.
treCorde            :: LyCmdPedal
treCorde            = cmdPedal "treCorde"

-- | @\\sostenutoDown@.
sostenutoDown       :: LyCmdPedal
sostenutoDown       = cmdPedal "sostenutoDown"

-- | @\\sostenutoUp@.
sostenutoUp         :: LyCmdPedal
sostenutoUp         = cmdPedal "sostenutoUp"


--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

-- | @acousticbassdrum@.
acousticbassdrum    :: LyDrumPitchName
acousticbassdrum    = drumPitchName "acousticbassdrum"

-- | @bassdrum@.
bassdrum            :: LyDrumPitchName
bassdrum            = drumPitchName "bassdrum"

-- | @hisidestick@.
hisidestick         :: LyDrumPitchName
hisidestick         = drumPitchName "hisidestick"

-- | @sidestick@.
sidestick           :: LyDrumPitchName
sidestick           = drumPitchName "sidestick"

-- | @losidestick@.
losidestick         :: LyDrumPitchName
losidestick         = drumPitchName "losidestick"

-- | @acousticsnare@.
acousticsnare       :: LyDrumPitchName
acousticsnare       = drumPitchName "acousticbassdrum"

-- | @snare@.
snare               :: LyDrumPitchName
snare               = drumPitchName "snare"

-- | @handclap@.
handclap            :: LyDrumPitchName
handclap            = drumPitchName "handclap"

-- | @electricsnare@.
electricsnare       :: LyDrumPitchName
electricsnare       = drumPitchName "electricsnare"

-- | @lowfloortom@.
lowfloortom         :: LyDrumPitchName
lowfloortom         = drumPitchName "lowfloortom"

-- | @closedhihat@.
closedhihat         :: LyDrumPitchName
closedhihat         = drumPitchName "closedhihat"

-- | @hihat@.
hihat               :: LyDrumPitchName
hihat               = drumPitchName "hihat"

-- | @highfloortom@.
highfloortom        :: LyDrumPitchName
highfloortom        = drumPitchName "highfloortom"

-- | @pedalhihat@.
pedalhihat          :: LyDrumPitchName
pedalhihat          = drumPitchName "pedalhihat"

-- | @lowtom@.
lowtom              :: LyDrumPitchName
lowtom              = drumPitchName "lowtom"

-- | @openhihat@.
openhihat           :: LyDrumPitchName
openhihat           = drumPitchName "openhihat"

-- | @halfopenhihat@.
halfopenhihat       :: LyDrumPitchName
halfopenhihat       = drumPitchName "halfopenhihat"

-- | @lowmidtom@.
lowmidtom           :: LyDrumPitchName
lowmidtom           = drumPitchName "lowmidtom"

-- | @himidtom@.
himidtom            :: LyDrumPitchName
himidtom            = drumPitchName "himidtom"

-- | @crashcymbala@.
crashcymbala        :: LyDrumPitchName
crashcymbala        = drumPitchName "crashcymbala"

-- | @crashcymbal@.
crashcymbal         :: LyDrumPitchName
crashcymbal         = drumPitchName "crashcymbal"

-- | @hightom@.
hightom             :: LyDrumPitchName
hightom             = drumPitchName "hightom"

-- | @ridecymbala@.

ridecymbala         :: LyDrumPitchName
ridecymbala         = drumPitchName "ridecymbala"

-- | @ridecymbal@.
ridecymbal          :: LyDrumPitchName
ridecymbal          = drumPitchName "ridecymbal"

-- | @chinesecymbal@.
chinesecymbal       :: LyDrumPitchName
chinesecymbal       = drumPitchName "chinesecymbal"

-- | @ridebell@.
ridebell            :: LyDrumPitchName
ridebell            = drumPitchName "ridebell"

-- | @tambourine@.
tambourine          :: LyDrumPitchName
tambourine          = drumPitchName "tambourine"

-- | @splashcymbal@.
splashcymbal        :: LyDrumPitchName
splashcymbal        = drumPitchName "splashcymbal"

-- | @cowbell@.
cowbell             :: LyDrumPitchName
cowbell             = drumPitchName "cowbell"

-- | @crashcymbalb@.
crashcymbalb        :: LyDrumPitchName
crashcymbalb        = drumPitchName "crashcymbalb"

-- | @vibraslap@.
vibraslap           :: LyDrumPitchName
vibraslap           = drumPitchName "vibraslap"

-- | @ridecymbalb@.
ridecymbalb         :: LyDrumPitchName
ridecymbalb         = drumPitchName "ridecymbalb"

-- | @mutehibongo@.
mutehibongo         :: LyDrumPitchName
mutehibongo         = drumPitchName "mutehibongo"

-- | @hibongo@.
hibongo             :: LyDrumPitchName
hibongo             = drumPitchName "hibongo"

-- | @openhibongo@.
openhibongo         :: LyDrumPitchName
openhibongo         = drumPitchName "openhibongo"

-- | @mutelobongo@.
mutelobongo         :: LyDrumPitchName
mutelobongo         = drumPitchName "mutelobongo"

-- | @lobongo@.
lobongo             :: LyDrumPitchName
lobongo             = drumPitchName "lobongo"

-- | @openlobongo@.
openlobongo         :: LyDrumPitchName
openlobongo         = drumPitchName "openlobongo"

-- | @mutehiconga@.
mutehiconga         :: LyDrumPitchName
mutehiconga         = drumPitchName "mutehiconga"

-- | @muteloconga@.
muteloconga         :: LyDrumPitchName
muteloconga         = drumPitchName "muteloconga"

-- | @openhiconga@.
openhiconga         :: LyDrumPitchName
openhiconga         = drumPitchName "openhiconga"

-- | @hiconga@.
hiconga             :: LyDrumPitchName
hiconga             = drumPitchName "hiconga"

-- | @openloconga@.
openloconga         :: LyDrumPitchName
openloconga         = drumPitchName "openloconga"

-- | @loconga@.
loconga             :: LyDrumPitchName
loconga             = drumPitchName "loconga"

-- | @hitimbale@.
hitimbale           :: LyDrumPitchName
hitimbale           = drumPitchName "hitimbale"

-- | @lotimbale@.
lotimbale           :: LyDrumPitchName
lotimbale           = drumPitchName "lotimbale"

-- | @hiagogo@.
hiagogo             :: LyDrumPitchName
hiagogo             = drumPitchName "hiagogo"

-- | @loagogo@.
loagogo             :: LyDrumPitchName
loagogo             = drumPitchName "loagogo"

-- | @cabasa@.
cabasa              :: LyDrumPitchName
cabasa              = drumPitchName "cabasa"

-- | @maracas@.
maracas             :: LyDrumPitchName
maracas             = drumPitchName "maracas"

-- | @shortwhistle@.
shortwhistle        :: LyDrumPitchName
shortwhistle        = drumPitchName "shortwhistle"

-- | @longwhistle@.
longwhistle         :: LyDrumPitchName
longwhistle         = drumPitchName "longwhistle"

-- | @shortguiro@.
shortguiro          :: LyDrumPitchName
shortguiro          = drumPitchName "shortguiro"

-- | @longguiro@.
longguiro           :: LyDrumPitchName
longguiro           = drumPitchName "longguiro"

-- | @guiro@.
guiro               :: LyDrumPitchName
guiro               = drumPitchName "guiro"

-- | @claves@.
claves              :: LyDrumPitchName
claves              = drumPitchName "claves"

-- | @hiwoodblock@.
hiwoodblock         :: LyDrumPitchName
hiwoodblock         = drumPitchName "hiwoodblock"

-- | @lowoodblock@.
lowoodblock         :: LyDrumPitchName
lowoodblock         = drumPitchName "lowoodblock"

-- | @mutecuica@.
mutecuica           :: LyDrumPitchName
mutecuica           = drumPitchName "mutecuica"

-- | @opencuica@.
opencuica           :: LyDrumPitchName
opencuica           = drumPitchName "opencuica"

-- | @mutetriangle@.
mutetriangle        :: LyDrumPitchName
mutetriangle        = drumPitchName "mutetriangle"

-- | @triangle@.
triangle            :: LyDrumPitchName
triangle            = drumPitchName "triangle"

-- | @opentriangle@.
opentriangle        :: LyDrumPitchName
opentriangle        = drumPitchName "opentriangle"

-- | @oneup@.
oneup               :: LyDrumPitchName
oneup               = drumPitchName "oneup"

-- | @twoup@.
twoup               :: LyDrumPitchName
twoup               = drumPitchName "twoup"

-- | @threeup@.
threeup             :: LyDrumPitchName
threeup             = drumPitchName "threeup"

-- | @fourup@.
fourup              :: LyDrumPitchName
fourup              = drumPitchName "fourup"

-- | @fiveup@.
fiveup              :: LyDrumPitchName
fiveup              = drumPitchName "fiveup"

-- | @onedown@.
onedown             :: LyDrumPitchName
onedown             = drumPitchName "onedown"

-- | @twodown@.
twodown             :: LyDrumPitchName
twodown             = drumPitchName "twodown"

-- | @threedown@.
threedown           :: LyDrumPitchName
threedown           = drumPitchName "threedown"

-- | @fourdown@.
fourdown            :: LyDrumPitchName
fourdown            = drumPitchName "fourdown"

-- | @fivedown@.
fivedown            :: LyDrumPitchName
fivedown            = drumPitchName "fivedown"



-- | @bda@ - abbreviated name for 'acousticbassdrum'.
bda                 :: LyDrumPitchName
bda                 = drumPitchName "bda"

-- | @bd@ - abbreviated name for 'bassdrum'.
bd                  :: LyDrumPitchName
bd                  = drumPitchName "bd"

-- | @ssh@ - abbreviated name for 'hisidestick'.
ssh                 :: LyDrumPitchName
ssh                 = drumPitchName "ssh"

-- | @ss@ - abbreviated name for 'sidestick'.
ss                  :: LyDrumPitchName
ss                  = drumPitchName "ss"

-- | @ssl@ - abbreviated name for 'losidestick'.
ssl                 :: LyDrumPitchName
ssl                 = drumPitchName "ssl"

-- | @sna@ - abbreviated name for 'acousticsnare'.
sna                 :: LyDrumPitchName
sna                 = drumPitchName "sna"

-- | @sn@ - abbreviated name for 'snare'.
sn                  :: LyDrumPitchName
sn                  = drumPitchName "sn"

-- | @hc@ - abbreviated name for 'handclap'.
hc                  :: LyDrumPitchName
hc                  = drumPitchName "hc"

-- | @sne@ - abbreviated name for 'electricsnare'.
sne                 :: LyDrumPitchName
sne                 = drumPitchName "sne"

-- | @tomfl@ - abbreviated name for 'lowfloortom'.
tomfl               :: LyDrumPitchName
tomfl               = drumPitchName "tomfl"

-- | @hhc@ - abbreviated name for 'closedhihat'.
hhc                 :: LyDrumPitchName
hhc                 = drumPitchName "hhc"

-- | @hh@ - abbreviated name for 'hihat'.
hh                  :: LyDrumPitchName
hh                  = drumPitchName "hh"

-- | @tomfh@ - abbreviated name for 'highfloortom'.
tomfh               :: LyDrumPitchName
tomfh               = drumPitchName "tomfh"


-- | @hhp@ - abbreviated name for 'pedalhihat'.
hhp                 :: LyDrumPitchName
hhp                 = drumPitchName "hhp"

-- | @toml@ - abbreviated name for 'lowtom'.
toml                :: LyDrumPitchName
toml                = drumPitchName "toml"

-- | @hho@ - abbreviated name for 'openhihat'.
hho                 :: LyDrumPitchName
hho                 = drumPitchName "hho"

-- | @hhho@ - abbreviated name for 'halfopenhihat'.
hhho                :: LyDrumPitchName
hhho                = drumPitchName "hhho"

-- | @tomml@ - abbreviated name for 'lowmidtom'.
tomml               :: LyDrumPitchName
tomml               = drumPitchName "tomml"

-- | @tommh@ - abbreviated name for 'himidtom'.
tommh               :: LyDrumPitchName
tommh               = drumPitchName "tommh"

-- | @cymca@ - abbreviated name for 'crashcymbala'.
cymca               :: LyDrumPitchName
cymca               = drumPitchName "cymca"

-- | @cymc@ - abbreviated name for 'crashcymbal'.
cymc                :: LyDrumPitchName
cymc                = drumPitchName "cymc"

-- | @tomh@ - abbreviated name for 'hightom'.
tomh                :: LyDrumPitchName
tomh                = drumPitchName "tomh"

-- | @cymra@ - abbreviated name for 'ridecymbala'.
cymra               :: LyDrumPitchName
cymra               = drumPitchName "cymra"

-- | @cymr@ - abbreviated name for 'ridecymbal'.
cymr                :: LyDrumPitchName
cymr                = drumPitchName "cymr"

-- | @cymch@ - abbreviated name for 'chinesecymbal'.
cymch               :: LyDrumPitchName
cymch               = drumPitchName "cymch"

-- | @rb@ - abbreviated name for 'ridebell'.
rb                  :: LyDrumPitchName
rb                  = drumPitchName "rb"

-- | @tamb@ - abbreviated name for 'tambourine'.
tamb                :: LyDrumPitchName
tamb                = drumPitchName "tamb"

-- | @cyms@ - abbreviated name for 'splashcymbal'.
cyms                :: LyDrumPitchName
cyms                = drumPitchName "cyms"

-- | @cb@ - abbreviated name for 'cowbell'.
cb                  :: LyDrumPitchName
cb                  = drumPitchName "cb"

-- | @cymcb@ - abbreviated name for 'crashcymbalb'.
cymcb               :: LyDrumPitchName
cymcb               = drumPitchName "cymcb"

-- | @vibs@ - abbreviated name for 'vibraslap'.
vibs                :: LyDrumPitchName
vibs                = drumPitchName "vibs"

-- | @cymrb@ - abbreviated name for 'ridecymbalb'.
cymrb               :: LyDrumPitchName
cymrb               = drumPitchName "cymrb"

-- | @bohm@ - abbreviated name for 'mutehibongo'.
bohm                :: LyDrumPitchName
bohm                = drumPitchName "bohm"

-- | @boh@ - abbreviated name for 'hibongo'.
boh                 :: LyDrumPitchName
boh                 = drumPitchName "boh"

-- | @boho@ - abbreviated name for 'openhibongo'.
boho                :: LyDrumPitchName
boho                = drumPitchName "boho"

-- | @bolm@ - abbreviated name for 'mutelobongo'.
bolm                :: LyDrumPitchName
bolm                = drumPitchName "bolm"

-- | @bol@ - abbreviated name for 'lobongo'.
bol                 :: LyDrumPitchName
bol                 = drumPitchName "bol"

-- | @bolo@ - abbreviated name for 'openlobongo'.
bolo                :: LyDrumPitchName
bolo                = drumPitchName "bolo"

-- | @cghm@ - abbreviated name for 'mutehiconga'.
cghm                :: LyDrumPitchName
cghm                = drumPitchName "cghm"

-- | @cglm@ - abbreviated name for 'muteloconga'.
cglm                :: LyDrumPitchName
cglm                = drumPitchName "cglm"

-- | @cgho@ - abbreviated name for 'openhiconga'.
cgho                :: LyDrumPitchName
cgho                = drumPitchName "cgho"

-- | @cgh@ - abbreviated name for 'hiconga'.
cgh                 :: LyDrumPitchName
cgh                 = drumPitchName "cgh"

-- | @cglo@ - abbreviated name for 'openloconga'.
cglo                :: LyDrumPitchName
cglo                = drumPitchName "cglo"

-- | @cgl@ - abbreviated name for 'loconga'.
cgl                 :: LyDrumPitchName
cgl                 = drumPitchName "cgl"

-- | @timh@ - abbreviated name for 'hitimbale'.
timh                :: LyDrumPitchName
timh                = drumPitchName "timh"

-- | @timl@ - abbreviated name for 'lotimbale'.
timl                :: LyDrumPitchName
timl                = drumPitchName "timl"

-- | @agh@ - abbreviated name for 'hiagogo'.
agh                 :: LyDrumPitchName
agh                 = drumPitchName "agh"

-- | @agl@ - abbreviated name for 'loagogo'.
agl                 :: LyDrumPitchName
agl                 = drumPitchName "agl"

-- | @cab@ - abbreviated name for 'cabasa'.
cab                 :: LyDrumPitchName
cab                 = drumPitchName "cab"

-- | @mar@ - abbreviated name for 'maracas'.
mar                 :: LyDrumPitchName
mar                 = drumPitchName "mar"

-- | @whs@ - abbreviated name for 'shortwhistle'.
whs                 :: LyDrumPitchName
whs                 = drumPitchName "whs"

-- | @whl@ - abbreviated name for 'longwhistle'.
whl                 :: LyDrumPitchName
whl                 = drumPitchName "whl"

-- | @guis@ - abbreviated name for 'shortguiro'.
guis                :: LyDrumPitchName
guis                = drumPitchName "guis"

-- | @guil@ - abbreviated name for 'longguiro'.
guil                :: LyDrumPitchName
guil                = drumPitchName "guil"

-- | @gui@ - abbreviated name for 'guiro'.
gui                 :: LyDrumPitchName
gui                 = drumPitchName "gui"

-- | @cl@ - abbreviated name for 'claves'.
cl                  :: LyDrumPitchName
cl                  = drumPitchName "cl"

-- | @wbh@ - abbreviated name for 'hiwoodblock'.
wbh                 :: LyDrumPitchName
wbh                 = drumPitchName "wbh"

-- | @wbl@ - abbreviated name for 'lowoodblock'.
wbl                 :: LyDrumPitchName
wbl                 = drumPitchName "wbl"

-- | @cuim@ - abbreviated name for 'mutecuica'.
cuim                :: LyDrumPitchName
cuim                = drumPitchName "cuim"

-- | @cuio@ - abbreviated name for 'opencuica'.
cuio                :: LyDrumPitchName
cuio                = drumPitchName "cuio"


-- | @trim@ - abbreviated name for 'mutetriangle'.
trim                :: LyDrumPitchName
trim                = drumPitchName "trim"

-- | @tri@ - abbreviated name for 'triangle'.
tri                 :: LyDrumPitchName
tri                 = drumPitchName "tri"

-- | @trio@ - abbreviated name for 'opentriangle'.
trio                :: LyDrumPitchName
trio                = drumPitchName "trio"

-- | @tt@ - abbreviated name for 'tamtam'.
tt                  :: LyDrumPitchName
tt                  = drumPitchName "tt"

-- | @ua@ - abbreviated name for 'oneup'.
ua                  :: LyDrumPitchName
ua                  = drumPitchName "ua"

-- | @ub@ - abbreviated name for 'twoup'.
ub                  :: LyDrumPitchName
ub                  = drumPitchName "ub"

-- | @uc@ - abbreviated name for 'threeup'.
uc                  :: LyDrumPitchName
uc                  = drumPitchName "uc"

-- | @ud@ - abbreviated name for 'fourup'.
ud                  :: LyDrumPitchName
ud                  = drumPitchName "ud"

-- | @ue@ - abbreviated name for 'fiveup'.
ue                  :: LyDrumPitchName
ue                  = drumPitchName "ue"

-- | @da@ - abbreviated name for 'onedown'.
da                  :: LyDrumPitchName
da                  = drumPitchName "da"

-- | @db@ - abbreviated name for 'twodown'.
db                  :: LyDrumPitchName
db                  = drumPitchName "db"

-- | @dc@ - abbreviated name for 'threedown'.
dc                  :: LyDrumPitchName
dc                  = drumPitchName "dc"

-- | @dd@ - abbreviated name for 'fourdown'.
dd                  :: LyDrumPitchName
dd                  = drumPitchName "dd"

-- | @de@ - abbreviated name for 'fivedown'.
de                  :: LyDrumPitchName
de                  = drumPitchName "de"


--------------------------------------------------------------------------------
-- ** Titles and headers (10.2)
-- *** Creating titles (10.2.1)

-- | @dedication@.
dedication          :: String -> LyHeaderElement
dedication          = headerElement "dedication"

-- | @title@.
title               :: String -> LyHeaderElement
title               = headerElement "title"

-- | @subtitle@.
subtitle            :: String -> LyHeaderElement
subtitle            = headerElement "subtitle"

-- | @subsubtitle@.
subsubtitle         :: String -> LyHeaderElement
subsubtitle         = headerElement "subsubtitle"

-- | @poet@.
poet                :: String -> LyHeaderElement
poet                = headerElement "poet"

-- | @composer@.
composer            :: String -> LyHeaderElement
composer            = headerElement "composer"

-- | @meter@.
meter               :: String -> LyHeaderElement
meter               = headerElement "meter"

-- | @opus@.
opus                :: String -> LyHeaderElement
opus                = headerElement "opus"

-- | @arranger@.
arranger            :: String -> LyHeaderElement
arranger            = headerElement "arranger"

-- | @instrument@.
instrument          :: String -> LyHeaderElement
instrument          = headerElement "arranger"

-- | @piece@.
piece               :: String -> LyHeaderElement
piece               = headerElement "piece"


-- | @copyright@.
copyright           :: String -> LyHeaderElement
copyright           = headerElement "copyright"

-- | @tagline@.
tagline             :: String -> LyHeaderElement
tagline             = headerElement "tagline"

-- | @breakbefore@.
breakbefore         :: Bool -> LyHeaderElement
breakbefore True    = equation "breakbefore" (wrap $ text "##t")
breakbefore False   = equation "breakbefore" (wrap $ text "##f")

