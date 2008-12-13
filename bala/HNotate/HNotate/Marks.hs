{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

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

data DrumMark

drumGrouping          :: Mark DrumMark -> Duration -> Grouping 
drumGrouping m d      = Singleton $ RhythmicMark "Drum" d m 

drumnote :: Mark DrumMark -> Duration -> EventList -> EventList
drumnote m d t = t |*> SingleE (drumGrouping m d) 

drumChordGrouping       :: [Mark DrumMark] -> Duration -> Grouping 
drumChordGrouping ms d  = 
    Singleton $ RhythmicMark "DrumChord" d (makeLyDrumChord ms)

drumchord :: [Mark DrumMark] -> Duration -> EventList -> EventList
drumchord ms d t = t |*> SingleE (drumChordGrouping ms d) 


makeLyDrum :: String -> Mark DrumMark
makeLyDrum name = Marker {_ly_output = text name, _abc_output = emptyDoc }

makeLyDrumChord :: [Mark DrumMark] -> Mark DrumMark
makeLyDrumChord xs = Marker { _ly_output = fn xs, _abc_output = emptyDoc }
  where
    fn = angles . hsep . fmap _ly_output  


                          
acousticbassdrum      :: Mark DrumMark
acousticbassdrum'     :: Mark DrumMark
acousticbassdrum      = makeLyDrum "acousticbassdrum" 
acousticbassdrum'     = makeLyDrum "bda"

bassdrum              :: Mark DrumMark                         
bassdrum'             :: Mark DrumMark
bassdrum              = makeLyDrum "bassdrum" 
bassdrum'             = makeLyDrum "bd"

hisidestick           :: Mark DrumMark
hisidestick'          :: Mark DrumMark
hisidestick           = makeLyDrum "hisidestick" 
hisidestick'          = makeLyDrum "ssh"

sidestick             :: Mark DrumMark
sidestick'            :: Mark DrumMark
sidestick             = makeLyDrum "sidestick"
sidestick'            = makeLyDrum "ss"

losidestick           :: Mark DrumMark
losidestick'          :: Mark DrumMark
losidestick           = makeLyDrum "losidestick"
losidestick'          = makeLyDrum "ssl"

acousticsnare         :: Mark DrumMark
acousticsnare'        :: Mark DrumMark
acousticsnare         = makeLyDrum "acousticsnare"
acousticsnare'        = makeLyDrum "sna"

snare                 :: Mark DrumMark
snare'                :: Mark DrumMark
snare                 = makeLyDrum "snare"
snare'                = makeLyDrum "sn"

handclap              :: Mark DrumMark
handclap'             :: Mark DrumMark
handclap              = makeLyDrum "handclap"
handclap'             = makeLyDrum "hc"

electricsnare         :: Mark DrumMark
electricsnare'        :: Mark DrumMark
electricsnare         = makeLyDrum "electricsnare"
electricsnare'        = makeLyDrum "sne"

lowfloortom           :: Mark DrumMark
lowfloortom'          :: Mark DrumMark
lowfloortom           = makeLyDrum "lowfloortom"
lowfloortom'          = makeLyDrum "tomfl"

closedhihat           :: Mark DrumMark
closedhihat'          :: Mark DrumMark
closedhihat           = makeLyDrum "closedhihat"
closedhihat'          = makeLyDrum "hhc"

hihat                 :: Mark DrumMark
hihat'                :: Mark DrumMark
hihat                 = makeLyDrum "hihat"
hihat'                = makeLyDrum "hh"

highfloortom          :: Mark DrumMark
highfloortom'         :: Mark DrumMark
highfloortom          = makeLyDrum "highfloortom"
highfloortom'         = makeLyDrum "tomfh"

pedalhihat            :: Mark DrumMark
pedalhihat'           :: Mark DrumMark
pedalhihat            = makeLyDrum "pedalhihat"
pedalhihat'           = makeLyDrum "hhp"

lowtom                :: Mark DrumMark
lowtom'               :: Mark DrumMark
lowtom                = makeLyDrum "lowtom"
lowtom'               = makeLyDrum "toml"

openhihat             :: Mark DrumMark
openhihat'            :: Mark DrumMark
openhihat             = makeLyDrum "openhihat"
openhihat'            = makeLyDrum "hho"

halfopenhihat         :: Mark DrumMark
halfopenhihat'        :: Mark DrumMark
halfopenhihat         = makeLyDrum "halfopenhihat"
halfopenhihat'        = makeLyDrum "hhho"

lowmidtom             :: Mark DrumMark
lowmidtom'            :: Mark DrumMark
lowmidtom             = makeLyDrum "lowmidtom"
lowmidtom'            = makeLyDrum "tomml"

himidtom              :: Mark DrumMark
himidtom'             :: Mark DrumMark
himidtom              = makeLyDrum "himidtom"
himidtom'             = makeLyDrum "tommh"

crashcymbala          :: Mark DrumMark
crashcymbala'         :: Mark DrumMark
crashcymbala          = makeLyDrum "crashcymbala"
crashcymbala'         = makeLyDrum "cymca"

crashcymbal           :: Mark DrumMark
crashcymbal'          :: Mark DrumMark
crashcymbal           = makeLyDrum "crashcymbal"
crashcymbal'          = makeLyDrum "cymc"

hightom               :: Mark DrumMark
hightom'              :: Mark DrumMark
hightom               = makeLyDrum "hightom"
hightom'              = makeLyDrum "tomh"

ridecymbala           :: Mark DrumMark
ridecymbala'          :: Mark DrumMark
ridecymbala           = makeLyDrum "ridecymbala"
ridecymbala'          = makeLyDrum "cymra"

ridecymbal            :: Mark DrumMark
ridecymbal'           :: Mark DrumMark
ridecymbal            = makeLyDrum "ridecymbal"
ridecymbal'           = makeLyDrum "cymr"

chinesecymbal         :: Mark DrumMark
chinesecymbal'        :: Mark DrumMark
chinesecymbal         = makeLyDrum "chinesecymbal"
chinesecymbal'        = makeLyDrum "cymch"

ridebell              :: Mark DrumMark
ridebell'             :: Mark DrumMark
ridebell              = makeLyDrum "ridebell"
ridebell'             = makeLyDrum "rb"

tambourine            :: Mark DrumMark
tambourine'           :: Mark DrumMark
tambourine            = makeLyDrum "tambourine"
tambourine'           = makeLyDrum "tamb"

splashcymbal          :: Mark DrumMark
splashcymbal'         :: Mark DrumMark
splashcymbal          = makeLyDrum "splashcymbal"
splashcymbal'         = makeLyDrum "cyms"

cowbell               :: Mark DrumMark
cowbell'              :: Mark DrumMark
cowbell               = makeLyDrum "cowbell"
cowbell'              = makeLyDrum "cb"

crashcymbalb          :: Mark DrumMark
crashcymbalb'         :: Mark DrumMark
crashcymbalb          = makeLyDrum "crashcymbalb"
crashcymbalb'         = makeLyDrum "cymcb"

vibraslap             :: Mark DrumMark
vibraslap'            :: Mark DrumMark
vibraslap             = makeLyDrum "vibraslap"
vibraslap'            = makeLyDrum "vibs"

ridecymbalb           :: Mark DrumMark
ridecymbalb'          :: Mark DrumMark
ridecymbalb           = makeLyDrum "ridecymbalb"
ridecymbalb'          = makeLyDrum "cymrb"

mutehibongo           :: Mark DrumMark
mutehibongo'          :: Mark DrumMark
mutehibongo           = makeLyDrum "mutehibongo"
mutehibongo'          = makeLyDrum "bohm"

hibongo               :: Mark DrumMark
hibongo'              :: Mark DrumMark
hibongo               = makeLyDrum "hibongo"
hibongo'              = makeLyDrum "boh"

openhibongo           :: Mark DrumMark
openhibongo'          :: Mark DrumMark
openhibongo           = makeLyDrum "openhibongo"
openhibongo'          = makeLyDrum "boho"

mutelobongo           :: Mark DrumMark
mutelobongo'          :: Mark DrumMark
mutelobongo           = makeLyDrum "mutelobongo"
mutelobongo'          = makeLyDrum "bolm"

lobongo               :: Mark DrumMark
lobongo'              :: Mark DrumMark
lobongo               = makeLyDrum "lobongo"
lobongo'              = makeLyDrum "bol"

openlobongo           :: Mark DrumMark
openlobongo'          :: Mark DrumMark
openlobongo           = makeLyDrum "openlobongo"
openlobongo'          = makeLyDrum "bolo"

mutehiconga           :: Mark DrumMark
mutehiconga'          :: Mark DrumMark
mutehiconga           = makeLyDrum "mutehiconga"
mutehiconga'          = makeLyDrum "cghm"

muteloconga           :: Mark DrumMark
muteloconga'          :: Mark DrumMark
muteloconga           = makeLyDrum "muteloconga"
muteloconga'          = makeLyDrum "cglm"

openhiconga           :: Mark DrumMark
openhiconga'          :: Mark DrumMark
openhiconga           = makeLyDrum "openhiconga"
openhiconga'          = makeLyDrum "cgho"

hiconga               :: Mark DrumMark
hiconga'              :: Mark DrumMark
hiconga               = makeLyDrum "hiconga"
hiconga'              = makeLyDrum "cgh"

openloconga           :: Mark DrumMark
openloconga'          :: Mark DrumMark
openloconga           = makeLyDrum "openloconga"
openloconga'          = makeLyDrum "cglo"

loconga               :: Mark DrumMark
loconga'              :: Mark DrumMark
loconga               = makeLyDrum "loconga"
loconga'              = makeLyDrum "cgl"

hitimbale             :: Mark DrumMark
hitimbale'            :: Mark DrumMark
hitimbale             = makeLyDrum "hitimbale"
hitimbale'            = makeLyDrum "timh"

lotimbale             :: Mark DrumMark
lotimbale'            :: Mark DrumMark
lotimbale             = makeLyDrum "lotimbale"
lotimbale'            = makeLyDrum "timl"

hiagogo               :: Mark DrumMark
hiagogo'              :: Mark DrumMark
hiagogo               = makeLyDrum "hiagogo"
hiagogo'              = makeLyDrum "agh"

loagogo               :: Mark DrumMark
loagogo'              :: Mark DrumMark
loagogo               = makeLyDrum "loagogo"
loagogo'              = makeLyDrum "agl"

cabasa                :: Mark DrumMark
cabasa'               :: Mark DrumMark
cabasa                = makeLyDrum "cabasa"
cabasa'               = makeLyDrum "cab"

maracas               :: Mark DrumMark
maracas'              :: Mark DrumMark
maracas               = makeLyDrum "maracas"
maracas'              = makeLyDrum "mar"

shortwhistle          :: Mark DrumMark
shortwhistle'         :: Mark DrumMark
shortwhistle          = makeLyDrum "shortwhistle"
shortwhistle'         = makeLyDrum "whs"

longwhistle           :: Mark DrumMark
longwhistle'          :: Mark DrumMark
longwhistle           = makeLyDrum "longwhistle"
longwhistle'          = makeLyDrum "whl"

shortguiro            :: Mark DrumMark
shortguiro'           :: Mark DrumMark
shortguiro            = makeLyDrum "shortguiro"
shortguiro'           = makeLyDrum "guis"

longguiro             :: Mark DrumMark
longguiro'            :: Mark DrumMark
longguiro             = makeLyDrum "longguiro"
longguiro'            = makeLyDrum "guil"

guiro                 :: Mark DrumMark
guiro'                :: Mark DrumMark
guiro                 = makeLyDrum "guiro"
guiro'                = makeLyDrum "gui"

claves                :: Mark DrumMark
claves'               :: Mark DrumMark
claves                = makeLyDrum "claves"
claves'               = makeLyDrum "cl"

hiwoodblock           :: Mark DrumMark
hiwoodblock'          :: Mark DrumMark
hiwoodblock           = makeLyDrum "hiwoodblock"
hiwoodblock'          = makeLyDrum "whi"

lowoodblock           :: Mark DrumMark
lowoodblock'          :: Mark DrumMark
lowoodblock           = makeLyDrum "lowoodblock"
lowoodblock'          = makeLyDrum "wbl"

mutecuica             :: Mark DrumMark
mutecuica'            :: Mark DrumMark
mutecuica             = makeLyDrum "mutecuica"
mutecuica'            = makeLyDrum "cuim"

opencuica             :: Mark DrumMark
opencuica'            :: Mark DrumMark
opencuica             = makeLyDrum "opencuica"
opencuica'            = makeLyDrum "cuio"

mutetriangle          :: Mark DrumMark
mutetriangle'         :: Mark DrumMark
mutetriangle          = makeLyDrum "mutetriangle"
mutetriangle'         = makeLyDrum "trim"

triangle              :: Mark DrumMark
triangle'             :: Mark DrumMark
triangle              = makeLyDrum "triangle"
triangle'             = makeLyDrum "tri"

opentriangle          :: Mark DrumMark
opentriangle'         :: Mark DrumMark
opentriangle          = makeLyDrum "opentriangle"
opentriangle'         = makeLyDrum "trio"

oneup                 :: Mark DrumMark
oneup'                :: Mark DrumMark
oneup                 = makeLyDrum "oneup"
oneup'                = makeLyDrum "ua"

twoup                 :: Mark DrumMark
twoup'                :: Mark DrumMark
twoup                 = makeLyDrum "twoup"
twoup'                = makeLyDrum "ub"

threeup               :: Mark DrumMark
threeup'              :: Mark DrumMark
threeup               = makeLyDrum "threeup"
threeup'              = makeLyDrum "uc"

fourup                :: Mark DrumMark
fourup'               :: Mark DrumMark
fourup                = makeLyDrum "fourup"
fourup'               = makeLyDrum "ud"

fiveup                :: Mark DrumMark
fiveup'               :: Mark DrumMark
fiveup                = makeLyDrum "fiveup"
fiveup'               = makeLyDrum "ue"

onedown               :: Mark DrumMark
onedown'              :: Mark DrumMark
onedown               = makeLyDrum "onedown"
onedown'              = makeLyDrum "da"

twodown               :: Mark DrumMark
twodown'              :: Mark DrumMark
twodown               = makeLyDrum "twodown"
twodown'              = makeLyDrum "db"

threedown             :: Mark DrumMark
threedown'            :: Mark DrumMark
threedown             = makeLyDrum "threedown"
threedown'            = makeLyDrum "dc"

fourdown              :: Mark DrumMark
fourdown'             :: Mark DrumMark
fourdown              = makeLyDrum "fourdown"
fourdown'             = makeLyDrum "dd"

fivedown              :: Mark DrumMark
fivedown'             :: Mark DrumMark
fivedown              = makeLyDrum "fivedown"     
fivedown'             = makeLyDrum "de"

