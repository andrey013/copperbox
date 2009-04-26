{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondPercussion
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utils...
--
--------------------------------------------------------------------------------

module Mullein.LilyPondPercussion where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LilyPondNoteClass
import Mullein.LilyPondOutput

import Text.PrettyPrint.Leijen 

data DrumPitch = DrumPitch { 
      drum_long_name   :: String, 
      drum_short_name  :: String 
    }
  deriving (Eq,Show)


type DrumNote = ElementP DrumPitch

perc :: DrumPitch -> Duration -> DrumNote
perc dp d    = Note dp d

drum :: String -> String -> Duration -> DrumNote
drum long short = perc (DrumPitch long short)

instance LyNote DrumPitch where
  rewritePitch = return
  rewritePitches = return
  lyNote p od = text (drum_short_name p) <> optDuration od
  lyPitch p   = text $ drum_short_name p


acousticbassdrum      :: Duration -> DrumNote
acousticbassdrum      = drum "acousticbassdrum" "bda"

bassdrum              :: Duration -> DrumNote                         
bassdrum              = drum "bassdrum" "bd"

hisidestick           :: Duration -> DrumNote
hisidestick           = drum "hisidestick" "ssh"

sidestick             :: Duration -> DrumNote
sidestick             = drum "sidestick" "ss"

losidestick           :: Duration -> DrumNote
losidestick           = drum "losidestick" "ssl"

acousticsnare         :: Duration -> DrumNote
acousticsnare         = drum "acousticsnare" "sna"

snare                 :: Duration -> DrumNote
snare                 = drum "snare" "sn"

handclap              :: Duration -> DrumNote
handclap              = drum "handclap" "hc"

electricsnare         :: Duration -> DrumNote
electricsnare         = drum "electricsnare" "sne"

lowfloortom           :: Duration -> DrumNote
lowfloortom           = drum "lowfloortom" "tomfl"

closedhihat           :: Duration -> DrumNote
closedhihat           = drum "closedhihat" "hhc"

hihat                 :: Duration -> DrumNote
hihat                 = drum "hihat" "hh"

highfloortom          :: Duration -> DrumNote
highfloortom          = drum "highfloortom" "tomfh"

pedalhihat            :: Duration -> DrumNote
pedalhihat            = drum "pedalhihat" "hhp"

lowtom                :: Duration -> DrumNote
lowtom                = drum "lowtom" "toml"

openhihat             :: Duration -> DrumNote
openhihat             = drum "openhihat" "hho"

halfopenhihat         :: Duration -> DrumNote
halfopenhihat         = drum "halfopenhihat" "hhho"

lowmidtom             :: Duration -> DrumNote
lowmidtom             = drum "lowmidtom" "tomml"

himidtom              :: Duration -> DrumNote
himidtom              = drum "himidtom" "tommh"

crashcymbala          :: Duration -> DrumNote
crashcymbala          = drum "crashcymbala" "cymca"

crashcymbal           :: Duration -> DrumNote
crashcymbal           = drum "crashcymbal" "cymc"

hightom               :: Duration -> DrumNote
hightom               = drum "hightom" "tomh"

ridecymbala           :: Duration -> DrumNote
ridecymbala           = drum "ridecymbala" "cymra"

ridecymbal            :: Duration -> DrumNote
ridecymbal            = drum "ridecymbal" "cymr"

chinesecymbal         :: Duration -> DrumNote
chinesecymbal         = drum "chinesecymbal" "cymch"

ridebell              :: Duration -> DrumNote
ridebell              = drum "ridebell" "rb"

tambourine            :: Duration -> DrumNote
tambourine            = drum "tambourine" "tamb"

splashcymbal          :: Duration -> DrumNote
splashcymbal          = drum "splashcymbal" "cyms"

cowbell               :: Duration -> DrumNote
cowbell               = drum "cowbell" "cb"

crashcymbalb          :: Duration -> DrumNote
crashcymbalb          = drum "crashcymbalb" "cymcb"

vibraslap             :: Duration -> DrumNote
vibraslap             = drum "vibraslap" "vibs"

ridecymbalb           :: Duration -> DrumNote
ridecymbalb           = drum "ridecymbalb" "cymrb"

mutehibongo           :: Duration -> DrumNote
mutehibongo           = drum "mutehibongo" "bohm"

hibongo               :: Duration -> DrumNote
hibongo               = drum "hibongo" "boh"

openhibongo           :: Duration -> DrumNote
openhibongo           = drum "openhibongo" "boho"

mutelobongo           :: Duration -> DrumNote
mutelobongo           = drum "mutelobongo" "bolm"

lobongo               :: Duration -> DrumNote
lobongo               = drum "lobongo" "bol"

openlobongo           :: Duration -> DrumNote
openlobongo           = drum "openlobongo" "bolo"

mutehiconga           :: Duration -> DrumNote
mutehiconga           = drum "mutehiconga" "cghm"

muteloconga           :: Duration -> DrumNote
muteloconga           = drum "muteloconga" "cglm"

openhiconga           :: Duration -> DrumNote
openhiconga           = drum "openhiconga" "cgho"

hiconga               :: Duration -> DrumNote
hiconga               = drum "hiconga" "cgh"

openloconga           :: Duration -> DrumNote
openloconga           = drum "openloconga" "cglo"

loconga               :: Duration -> DrumNote
loconga               = drum "loconga" "cgl"

hitimbale             :: Duration -> DrumNote
hitimbale             = drum "hitimbale" "timh"

lotimbale             :: Duration -> DrumNote
lotimbale             = drum "lotimbale" "timl"

hiagogo               :: Duration -> DrumNote
hiagogo               = drum "hiagogo" "agh"

loagogo               :: Duration -> DrumNote
loagogo               = drum "loagogo" "agl"

cabasa                :: Duration -> DrumNote
cabasa                = drum "cabasa" "cab"

maracas               :: Duration -> DrumNote
maracas               = drum "maracas" "mar"

shortwhistle          :: Duration -> DrumNote
shortwhistle          = drum "shortwhistle" "whs"

longwhistle           :: Duration -> DrumNote
longwhistle           = drum "longwhistle" "whl"

shortguiro            :: Duration -> DrumNote
shortguiro            = drum "shortguiro" "guis"

longguiro             :: Duration -> DrumNote
longguiro             = drum "longguiro" "guil"

guiro                 :: Duration -> DrumNote
guiro                 = drum "guiro" "gui"

claves                :: Duration -> DrumNote
claves                = drum "claves" "cl"

hiwoodblock           :: Duration -> DrumNote
hiwoodblock           = drum "hiwoodblock" "whi"

lowoodblock           :: Duration -> DrumNote
lowoodblock           = drum "lowoodblock" "wbl"

mutecuica             :: Duration -> DrumNote
mutecuica             = drum "mutecuica" "cuim"

opencuica             :: Duration -> DrumNote
opencuica             = drum "opencuica" "cuio"

mutetriangle          :: Duration -> DrumNote
mutetriangle          = drum "mutetriangle" "trim"

triangle              :: Duration -> DrumNote
triangle              = drum "triangle" "tri"

opentriangle          :: Duration -> DrumNote
opentriangle          = drum "opentriangle" "trio"

oneup                 :: Duration -> DrumNote
oneup                 = drum "oneup" "ua"

twoup                 :: Duration -> DrumNote
twoup                 = drum "twoup" "ub"

threeup               :: Duration -> DrumNote
threeup               = drum "threeup" "uc"

fourup                :: Duration -> DrumNote
fourup                = drum "fourup" "ud"

fiveup                :: Duration -> DrumNote
fiveup                = drum "fiveup" "ue"

onedown               :: Duration -> DrumNote
onedown               = drum "onedown" "da"

twodown               :: Duration -> DrumNote
twodown               = drum "twodown" "db"

threedown             :: Duration -> DrumNote
threedown             = drum "threedown" "dc"

fourdown              :: Duration -> DrumNote
fourdown              = drum "fourdown" "dd"

fivedown              :: Duration -> DrumNote
fivedown              = drum "fivedown" "de"


