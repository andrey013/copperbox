--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Marks
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common annotations for LilyPond and Abc 
--
--------------------------------------------------------------------------------

module HNotate.Marks where

import HNotate.Document
import HNotate.Duration
import HNotate.NoteListDatatypes

-- How do we represent 'drum chords'?
chordMark :: [Tile] -> Duration -> Tile
chordMark _ d = undefined

makeLyDrum :: String -> Duration -> Tile
makeLyDrum name d = Singleton $ RhythmicMark name d (M (fs name) ())
    
  where 
    fs ss = (\() -> MarkF { _ly_output = \() -> text ss,
                            _abc_output = \() -> emptyDoc })



                          
acousticbassdrum      :: Duration -> Tile
acousticbassdrum'     :: Duration -> Tile
acousticbassdrum      = makeLyDrum "acousticbassdrum" 
acousticbassdrum'     = makeLyDrum "bda"

bassdrum              :: Duration -> Tile                         
bassdrum'             :: Duration -> Tile
bassdrum              = makeLyDrum "bassdrum" 
bassdrum'             = makeLyDrum "bd"

hisidestick           :: Duration -> Tile
hisidestick'          :: Duration -> Tile
hisidestick           = makeLyDrum "hisidestick" 
hisidestick'          = makeLyDrum "ssh"

sidestick             :: Duration -> Tile
sidestick'            :: Duration -> Tile
sidestick             = makeLyDrum "sidestick"
sidestick'            = makeLyDrum "ss"

losidestick           :: Duration -> Tile
losidestick'          :: Duration -> Tile
losidestick           = makeLyDrum "losidestick"
losidestick'          = makeLyDrum "ssl"

acousticsnare         :: Duration -> Tile
acousticsnare'        :: Duration -> Tile
acousticsnare         = makeLyDrum "acousticsnare"
acousticsnare'        = makeLyDrum "sna"

snare                 :: Duration -> Tile
snare'                :: Duration -> Tile
snare                 = makeLyDrum "snare"
snare'                = makeLyDrum "sn"

handclap              :: Duration -> Tile
handclap'             :: Duration -> Tile
handclap              = makeLyDrum "handclap"
handclap'             = makeLyDrum "hc"

electricsnare         :: Duration -> Tile
electricsnare'        :: Duration -> Tile
electricsnare         = makeLyDrum "electricsnare"
electricsnare'        = makeLyDrum "sne"

lowfloortom           :: Duration -> Tile
lowfloortom'          :: Duration -> Tile
lowfloortom           = makeLyDrum "lowfloortom"
lowfloortom'          = makeLyDrum "tomfl"

closedhihat           :: Duration -> Tile
closedhihat'          :: Duration -> Tile
closedhihat           = makeLyDrum "closedhihat"
closedhihat'          = makeLyDrum "hhc"

hihat                 :: Duration -> Tile
hihat'                :: Duration -> Tile
hihat                 = makeLyDrum "hihat"
hihat'                = makeLyDrum "hh"

highfloortom          :: Duration -> Tile
highfloortom'         :: Duration -> Tile
highfloortom          = makeLyDrum "highfloortom"
highfloortom'         = makeLyDrum "tomfh"

pedalhihat            :: Duration -> Tile
pedalhihat'           :: Duration -> Tile
pedalhihat            = makeLyDrum "pedalhihat"
pedalhihat'           = makeLyDrum "hhp"

lowtom                :: Duration -> Tile
lowtom'               :: Duration -> Tile
lowtom                = makeLyDrum "lowtom"
lowtom'               = makeLyDrum "toml"

openhihat             :: Duration -> Tile
openhihat'            :: Duration -> Tile
openhihat             = makeLyDrum "openhihat"
openhihat'            = makeLyDrum "hho"

halfopenhihat         :: Duration -> Tile
halfopenhihat'        :: Duration -> Tile
halfopenhihat         = makeLyDrum "halfopenhihat"
halfopenhihat'        = makeLyDrum "hhho"

lowmidtom             :: Duration -> Tile
lowmidtom'            :: Duration -> Tile
lowmidtom             = makeLyDrum "lowmidtom"
lowmidtom'            = makeLyDrum "tomml"

himidtom              :: Duration -> Tile
himidtom'             :: Duration -> Tile
himidtom              = makeLyDrum "himidtom"
himidtom'             = makeLyDrum "tommh"

crashcymbala          :: Duration -> Tile
crashcymbala'         :: Duration -> Tile
crashcymbala          = makeLyDrum "crashcymbala"
crashcymbala'         = makeLyDrum "cymca"

crashcymbal           :: Duration -> Tile
crashcymbal'          :: Duration -> Tile
crashcymbal           = makeLyDrum "crashcymbal"
crashcymbal'          = makeLyDrum "cymc"

hightom               :: Duration -> Tile
hightom'              :: Duration -> Tile
hightom               = makeLyDrum "hightom"
hightom'              = makeLyDrum "tomh"

ridecymbala           :: Duration -> Tile
ridecymbala'          :: Duration -> Tile
ridecymbala           = makeLyDrum "ridecymbala"
ridecymbala'          = makeLyDrum "cymra"

ridecymbal            :: Duration -> Tile
ridecymbal'           :: Duration -> Tile
ridecymbal            = makeLyDrum "ridecymbal"
ridecymbal'           = makeLyDrum "cymr"

chinesecymbal         :: Duration -> Tile
chinesecymbal'        :: Duration -> Tile
chinesecymbal         = makeLyDrum "chinesecymbal"
chinesecymbal'        = makeLyDrum "cymch"

ridebell              :: Duration -> Tile
ridebell'             :: Duration -> Tile
ridebell              = makeLyDrum "ridebell"
ridebell'             = makeLyDrum "rb"

tambourine            :: Duration -> Tile
tambourine'           :: Duration -> Tile
tambourine            = makeLyDrum "tambourine"
tambourine'           = makeLyDrum "tamb"

splashcymbal          :: Duration -> Tile
splashcymbal'         :: Duration -> Tile
splashcymbal          = makeLyDrum "splashcymbal"
splashcymbal'         = makeLyDrum "cyms"

cowbell               :: Duration -> Tile
cowbell'              :: Duration -> Tile
cowbell               = makeLyDrum "cowbell"
cowbell'              = makeLyDrum "cb"

crashcymbalb          :: Duration -> Tile
crashcymbalb'         :: Duration -> Tile
crashcymbalb          = makeLyDrum "crashcymbalb"
crashcymbalb'         = makeLyDrum "cymcb"

vibraslap             :: Duration -> Tile
vibraslap'            :: Duration -> Tile
vibraslap             = makeLyDrum "vibraslap"
vibraslap'            = makeLyDrum "vibs"

ridecymbalb           :: Duration -> Tile
ridecymbalb'          :: Duration -> Tile
ridecymbalb           = makeLyDrum "ridecymbalb"
ridecymbalb'          = makeLyDrum "cymrb"

mutehibongo           :: Duration -> Tile
mutehibongo'          :: Duration -> Tile
mutehibongo           = makeLyDrum "mutehibongo"
mutehibongo'          = makeLyDrum "bohm"

hibongo               :: Duration -> Tile
hibongo'              :: Duration -> Tile
hibongo               = makeLyDrum "hibongo"
hibongo'              = makeLyDrum "boh"

openhibongo           :: Duration -> Tile
openhibongo'          :: Duration -> Tile
openhibongo           = makeLyDrum "openhibongo"
openhibongo'          = makeLyDrum "boho"

mutelobongo           :: Duration -> Tile
mutelobongo'          :: Duration -> Tile
mutelobongo           = makeLyDrum "mutelobongo"
mutelobongo'          = makeLyDrum "bolm"

lobongo               :: Duration -> Tile
lobongo'              :: Duration -> Tile
lobongo               = makeLyDrum "lobongo"
lobongo'              = makeLyDrum "bol"

openlobongo           :: Duration -> Tile
openlobongo'          :: Duration -> Tile
openlobongo           = makeLyDrum "openlobongo"
openlobongo'          = makeLyDrum "bolo"

mutehiconga           :: Duration -> Tile
mutehiconga'          :: Duration -> Tile
mutehiconga           = makeLyDrum "mutehiconga"
mutehiconga'          = makeLyDrum "cghm"

muteloconga           :: Duration -> Tile
muteloconga'          :: Duration -> Tile
muteloconga           = makeLyDrum "muteloconga"
muteloconga'          = makeLyDrum "cglm"

openhiconga           :: Duration -> Tile
openhiconga'          :: Duration -> Tile
openhiconga           = makeLyDrum "openhiconga"
openhiconga'          = makeLyDrum "cgho"

hiconga               :: Duration -> Tile
hiconga'              :: Duration -> Tile
hiconga               = makeLyDrum "hiconga"
hiconga'              = makeLyDrum "cgh"

openloconga           :: Duration -> Tile
openloconga'          :: Duration -> Tile
openloconga           = makeLyDrum "openloconga"
openloconga'          = makeLyDrum "cglo"

loconga               :: Duration -> Tile
loconga'              :: Duration -> Tile
loconga               = makeLyDrum "loconga"
loconga'              = makeLyDrum "cgl"

hitimbale             :: Duration -> Tile
hitimbale'            :: Duration -> Tile
hitimbale             = makeLyDrum "hitimbale"
hitimbale'            = makeLyDrum "timh"

lotimbale             :: Duration -> Tile
lotimbale'            :: Duration -> Tile
lotimbale             = makeLyDrum "lotimbale"
lotimbale'            = makeLyDrum "timl"

hiagogo               :: Duration -> Tile
hiagogo'              :: Duration -> Tile
hiagogo               = makeLyDrum "hiagogo"
hiagogo'              = makeLyDrum "agh"

loagogo               :: Duration -> Tile
loagogo'              :: Duration -> Tile
loagogo               = makeLyDrum "loagogo"
loagogo'              = makeLyDrum "agl"

cabasa                :: Duration -> Tile
cabasa'               :: Duration -> Tile
cabasa                = makeLyDrum "cabasa"
cabasa'               = makeLyDrum "cab"

maracas               :: Duration -> Tile
maracas'              :: Duration -> Tile
maracas               = makeLyDrum "maracas"
maracas'              = makeLyDrum "mar"

shortwhistle          :: Duration -> Tile
shortwhistle'         :: Duration -> Tile
shortwhistle          = makeLyDrum "shortwhistle"
shortwhistle'         = makeLyDrum "whs"

longwhistle           :: Duration -> Tile
longwhistle'          :: Duration -> Tile
longwhistle           = makeLyDrum "longwhistle"
longwhistle'          = makeLyDrum "whl"

shortguiro            :: Duration -> Tile
shortguiro'           :: Duration -> Tile
shortguiro            = makeLyDrum "shortguiro"
shortguiro'           = makeLyDrum "guis"

longguiro             :: Duration -> Tile
longguiro'            :: Duration -> Tile
longguiro             = makeLyDrum "longguiro"
longguiro'            = makeLyDrum "guil"

guiro                 :: Duration -> Tile
guiro'                :: Duration -> Tile
guiro                 = makeLyDrum "guiro"
guiro'                = makeLyDrum "gui"

claves                :: Duration -> Tile
claves'               :: Duration -> Tile
claves                = makeLyDrum "claves"
claves'               = makeLyDrum "cl"

hiwoodblock           :: Duration -> Tile
hiwoodblock'          :: Duration -> Tile
hiwoodblock           = makeLyDrum "hiwoodblock"
hiwoodblock'          = makeLyDrum "whi"

lowoodblock           :: Duration -> Tile
lowoodblock'          :: Duration -> Tile
lowoodblock           = makeLyDrum "lowoodblock"
lowoodblock'          = makeLyDrum "wbl"

mutecuica             :: Duration -> Tile
mutecuica'            :: Duration -> Tile
mutecuica             = makeLyDrum "mutecuica"
mutecuica'            = makeLyDrum "cuim"

opencuica             :: Duration -> Tile
opencuica'            :: Duration -> Tile
opencuica             = makeLyDrum "opencuica"
opencuica'            = makeLyDrum "cuio"

mutetriangle          :: Duration -> Tile
mutetriangle'         :: Duration -> Tile
mutetriangle          = makeLyDrum "mutetriangle"
mutetriangle'         = makeLyDrum "trim"

triangle              :: Duration -> Tile
triangle'             :: Duration -> Tile
triangle              = makeLyDrum "triangle"
triangle'             = makeLyDrum "tri"

opentriangle          :: Duration -> Tile
opentriangle'         :: Duration -> Tile
opentriangle          = makeLyDrum "opentriangle"
opentriangle'         = makeLyDrum "trio"

oneup                 :: Duration -> Tile
oneup'                :: Duration -> Tile
oneup                 = makeLyDrum "oneup"
oneup'                = makeLyDrum "ua"

twoup                 :: Duration -> Tile
twoup'                :: Duration -> Tile
twoup                 = makeLyDrum "twoup"
twoup'                = makeLyDrum "ub"

threeup               :: Duration -> Tile
threeup'              :: Duration -> Tile
threeup               = makeLyDrum "threeup"
threeup'              = makeLyDrum "uc"

fourup                :: Duration -> Tile
fourup'               :: Duration -> Tile
fourup                = makeLyDrum "fourup"
fourup'               = makeLyDrum "ud"

fiveup                :: Duration -> Tile
fiveup'               :: Duration -> Tile
fiveup                = makeLyDrum "fiveup"
fiveup'               = makeLyDrum "ue"

onedown               :: Duration -> Tile
onedown'              :: Duration -> Tile
onedown               = makeLyDrum "onedown"
onedown'              = makeLyDrum "da"

twodown               :: Duration -> Tile
twodown'              :: Duration -> Tile
twodown               = makeLyDrum "twodown"
twodown'              = makeLyDrum "db"

threedown             :: Duration -> Tile
threedown'            :: Duration -> Tile
threedown             = makeLyDrum "threedown"
threedown'            = makeLyDrum "dc"

fourdown              :: Duration -> Tile
fourdown'             :: Duration -> Tile
fourdown              = makeLyDrum "fourdown"
fourdown'             = makeLyDrum "dd"

fivedown              :: Duration -> Tile
fivedown'             :: Duration -> Tile
fivedown              = makeLyDrum "fivedown"     
fivedown'             = makeLyDrum "de"

